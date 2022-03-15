const std = @import("std");
const builtin = @import("builtin");

pub fn combinedEnumsFieldCount(comptime A: type, comptime B: type) comptime_int {
    return @typeInfo(A).Enum.fields.len * @typeInfo(B).Enum.fields.len;
}

pub fn CombinedEnums(comptime A: type, comptime B: type) type {
    const name_separator = "_".*;
    var new_fields: []const std.builtin.TypeInfo.EnumField = &[_]std.builtin.TypeInfo.EnumField{};

    const TagNamespace = CombinedEnumsTagNamespace(A, B);

    std.debug.assert(TagNamespace.util_a.info.is_exhaustive);
    std.debug.assert(TagNamespace.util_b.info.is_exhaustive);

    for (TagNamespace.util_a.info.fields) |field_a| {
        const a = @field(A, field_a.name);
        for (TagNamespace.util_b.info.fields) |field_b| {
            const b = @field(B, field_b.name);

            const tag = TagNamespace.tagCombined(a, b);
            new_fields = new_fields ++ [_]std.builtin.TypeInfo.EnumField{.{
                .name = @tagName(a) ++ name_separator ++ @tagName(b),
                .value = tag,
            }};
        }
    }

    return @Type(@unionInit(std.builtin.TypeInfo, "Enum", std.builtin.TypeInfo.Enum{
        .layout = .Auto,
        .tag_type = TagNamespace.TagCombined,
        .fields = new_fields,
        .decls = &[_]std.builtin.TypeInfo.Declaration{},
        .is_exhaustive = true,
    }));
}

pub fn combineEnums(a: anytype, b: anytype) CombinedEnums(@TypeOf(a), @TypeOf(b)) {
    const A = @TypeOf(a);
    const B = @TypeOf(b);
    const Combined = CombinedEnums(A, B);
    const TagNamespace = CombinedEnumsTagNamespace(A, B);

    // TODO: Remove this once casting to `u0` works.
    // This is a work around for casting 0 to `u0` crashing the compiler
    comptime if (TagNamespace.TagCombined == u0) {
        return @field(Combined, @typeInfo(Combined).Enum.fields[0].name);
    };

    comptime if (@typeInfo(Combined).Enum.fields.len == 1) {
        return @field(Combined, @typeInfo(Combined).Enum.fields[0].name);
    };

    return @intToEnum(Combined, TagNamespace.tagCombined(a, b));
}

pub fn combineEnumsTemplate(comptime A: type, comptime B: type) (fn (A, B) CombinedEnums(A, B)) {
    return comptime struct {
        fn combineEnumsFn(a: A, b: B) CombinedEnums(A, B) {
            return combineEnums(a, b);
        }
    }.combineEnumsFn;
}

fn EnumUtilNamespace(comptime Enum: type) type {
    return struct {
        const info: std.builtin.Type.Enum = @typeInfo(Enum).Enum;

        const Tag: type = info.tag_type;
        const bits: u16 = @typeInfo(Tag).Int.bits;

        const TagSigned: type = std.meta.Int(.signed, bits);
        const TagUnsigned: type = std.meta.Int(.unsigned, bits);

        const values: [info.fields.len]Enum = std.enums.values(Enum)[0..info.fields.len].*;
        const values_vec: @Vector(info.fields.len, Tag) = @bitCast(@Vector(info.fields.len, Tag), values);

        const value_min: Enum = @intToEnum(Enum, @reduce(.Min, values_vec));
        const value_max: Enum = @intToEnum(Enum, @reduce(.Max, values_vec));

        const AscIndex = std.math.IntFittingRange(0, info.fields.len - 1);
        /// Returns an index such that `tag == values_ascending[ascIndex(tag)]`.
        fn ascIndex(tag: Enum) AscIndex {
            var result: AscIndex = 0;
            inline for (values) |possible_tag| {
                result += @boolToInt((@enumToInt(tag) - @enumToInt(value_min)) > (@enumToInt(possible_tag) - @enumToInt(value_min)));
            }
            return result;
        }
    };
}

fn CombinedEnumsTagNamespace(comptime A: type, comptime B: type) type {
    return struct {
        const util_a = EnumUtilNamespace(A);
        const util_b = EnumUtilNamespace(B);
        const TagCombined = std.meta.Int(.unsigned, util_a.bits + util_b.bits);

        fn assertValidCombo() void {
            if (comptime util_a.values.len == 0 or util_b.values.len == 0) {
                @compileError("Cannot combine enums with zero members.");
            }
        }

        const lut: [util_a.values.len * util_b.values.len]TagCombined = lut: {
            var result: [util_a.values.len * util_b.values.len]TagCombined = undefined;

            for (util_b.values) |b| {
                const b_index = util_b.ascIndex(b);
                for (util_a.values) |a| {
                    const a_index = util_a.ascIndex(a);
                    const combined_index = a_index + (b_index * util_a.info.fields.len);
                    result[combined_index] = combined_index;
                }
            }

            break :lut result;
        };

        fn tagCombined(a: A, b: B) TagCombined {
            assertValidCombo();
            const a_index = util_a.ascIndex(a);
            const b_index = util_b.ascIndex(b);
            return lut[a_index + (b_index * util_a.values.len)];
        }

        fn extractTagA(combined: TagCombined) util_a.Tag {
            assertValidCombo();
            return @intCast(util_a.Tag, combined % util_a.values.len);
        }

        fn extractTagB(combined: TagCombined) util_b.Tag {
            assertValidCombo();
            return @intCast(util_b.Tag, @divTrunc(combined, util_a.values.len));
        }
    };
}

pub const SplitCombinedEnumComponents = enum { a, b, both };
pub fn splitCombinedEnumsTemplate(
    comptime A: type,
    comptime B: type,
    comptime split: SplitCombinedEnumComponents,
) fn (CombinedEnums(A, B)) switch (split) {
    .a => A,
    .b => B,
    .both => std.meta.ArgsTuple(@TypeOf(combineEnumsTemplate(A, B))),
} {
    const Combined = CombinedEnums(A, B);
    const ReturnType = switch (split) {
        .a => A,
        .b => B,
        .both => std.meta.ArgsTuple(@TypeOf(combineEnumsTemplate(A, B))),
    };

    const TagNamespace = CombinedEnumsTagNamespace(A, B);

    return comptime struct {
        fn splitCombinedEnum(combined: Combined) ReturnType {
            // TODO: Remove this once casting to `u0` works.
            // This is a work around for casting 0 to `u0` crashing the compiler
            comptime if (TagNamespace.TagCombined == u0) return switch (split) {
                .a, .b => std.enums.values(ReturnType)[0],
                .both => .{
                    .a = std.enums.values(A)[0],
                    .b = return std.enums.values(B)[0],
                },
            };

            return switch (comptime split) {
                .a => @intToEnum(A, CombinedEnumsTagNamespace(A, B).extractTagA(@enumToInt(combined))),
                .b => @intToEnum(B, CombinedEnumsTagNamespace(A, B).extractTagB(@enumToInt(combined))),
                .both => .{
                    @intToEnum(A, CombinedEnumsTagNamespace(A, B).extractTagA(@enumToInt(combined))),
                    @intToEnum(B, CombinedEnumsTagNamespace(A, B).extractTagB(@enumToInt(combined))),
                },
            };
        }
    }.splitCombinedEnum;
}

test "combineEnums" {
    {
        const Foo = enum { foo };
        const Bar = enum { bar };
        try std.testing.expectEqual(CombinedEnums(Foo, Bar).foo_bar, combineEnums(Foo.foo, Bar.bar));
        try std.testing.expectEqual(Foo.foo, splitCombinedEnumsTemplate(Foo, Bar, .a)(combineEnums(Foo.foo, Bar.bar)));
        try std.testing.expectEqual(Bar.bar, splitCombinedEnumsTemplate(Foo, Bar, .b)(combineEnums(Foo.foo, Bar.bar)));
    }

    {
        const Direction = enum(u8) { north, east, south, west };
        const Rotation = enum(u8) { clockwise, anticlockwise };
        const Combined = CombinedEnums(Direction, Rotation);

        const combineDirRot = comptime combineEnumsTemplate(Direction, Rotation);

        const getBoth = comptime splitCombinedEnumsTemplate(Direction, Rotation, .both);
        const getDir = comptime splitCombinedEnumsTemplate(Direction, Rotation, .a);
        const getRot = comptime splitCombinedEnumsTemplate(Direction, Rotation, .b);

        for (std.enums.values(Direction)) |expected_d| {
            for (std.enums.values(Rotation)) |expected_r| {
                const combined = combineDirRot(expected_d, expected_r);
                const actual_d = getDir(combined);
                const actual_r = getRot(combined);

                try std.testing.expectEqual(expected_d, actual_d);
                try std.testing.expectEqual(expected_r, actual_r);

                try std.testing.expectEqual(combineDirRot(expected_d, expected_r), combined);
                try std.testing.expectEqual(combined, @call(.{}, combineDirRot, getBoth(combined)));
            }
        }

        comptime try std.testing.expect(@hasField(Combined, "north_clockwise"));
        try std.testing.expectEqual(Combined.north_clockwise, combineDirRot(.north, .clockwise));
    }
}

/// Attempts to cast an enum to another enum, by name. Returns null
/// if the target type does not possess a name in common with the
/// castee.
pub fn tagNameCast(comptime T: type, from: anytype) ?T {
    const FromType = @TypeOf(from);

    if (FromType == @Type(.EnumLiteral)) {
        const tag_name = comptime @tagName(from);
        if (!@hasField(T, tag_name)) return null;
        return @field(T, tag_name);
    }

    inline for (comptime std.enums.values(FromType)) |active_tag| {
        if (active_tag == from) {
            const tag_name = @tagName(active_tag);
            if (!@hasField(T, tag_name)) return null;
            return @field(T, tag_name);
        }
    }

    return null;
}

test "tagNameCast" {
    const A = enum(u1) { a = 0, b = 1 };
    const B = enum(u1) { a = 1, b = 0 };
    try std.testing.expectEqual(@as(?A, .a), tagNameCast(A, .a));
    try std.testing.expectEqual(@as(?A, .a), tagNameCast(A, A.a));
    try std.testing.expectEqual(@as(?A, .a), tagNameCast(A, B.a));

    try std.testing.expectEqual(@as(?B, .a), tagNameCast(B, .a));
    try std.testing.expectEqual(@as(?B, .a), tagNameCast(B, A.a));
    try std.testing.expectEqual(@as(?B, .a), tagNameCast(B, B.a));

    try std.testing.expectEqual(@as(?B, .b), tagNameCast(B, .b));
    try std.testing.expectEqual(@as(?B, .b), tagNameCast(B, A.b));
    try std.testing.expectEqual(@as(?B, .b), tagNameCast(B, B.b));

    try std.testing.expectEqual(@as(?A, null), tagNameCast(A, .c));
    try std.testing.expectEqual(@as(?B, null), tagNameCast(B, .d));
    try std.testing.expectEqual(@as(?B, null), tagNameCast(B, enum { e }.e));
}
