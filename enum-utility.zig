const std = @import("std");
const builtin = @import("builtin");

pub fn combinedEnumsFieldCount(comptime A: type, comptime B: type) comptime_int {
    return @typeInfo(A).Enum.fields.len * @typeInfo(B).Enum.fields.len;
}

pub fn CombinedEnums(comptime A: type, comptime B: type) type {
    const name_separator = "_".*;
    var new_fields: []const std.builtin.TypeInfo.EnumField = &[_]std.builtin.TypeInfo.EnumField{};

    const TagNamespace = CombinedEnumsTagNamespace(A, B);

    std.debug.assert(TagNamespace.info_a.is_exhaustive);
    std.debug.assert(TagNamespace.info_b.is_exhaustive);

    for (TagNamespace.info_a.fields) |field_a| {
        const a = @field(A, field_a.name);
        for (TagNamespace.info_b.fields) |field_b| {
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

pub noinline fn combineEnums(a: anytype, b: anytype) CombinedEnums(@TypeOf(a), @TypeOf(b)) {
    const A = @TypeOf(a);
    const B = @TypeOf(b);
    const TagNamespace = CombinedEnumsTagNamespace(A, B);

    // TODO: Remove this once casting to `u0` works.
    // This is a work around for casting 0 to `u0` crashing the compiler
    comptime if (TagNamespace.TagCombined == u0) {
        return std.enums.values(CombinedEnums(A, B))[0];
    };

    return @intToEnum(CombinedEnums(A, B), TagNamespace.tagCombined(a, b));
}

pub fn combineEnumsTemplate(comptime A: type, comptime B: type) (fn (A, B) CombinedEnums(A, B)) {
    return comptime struct {
        fn combineEnumsFn(a: A, b: B) CombinedEnums(A, B) {
            return combineEnums(a, b);
        }
    }.combineEnumsFn;
}

fn CombinedEnumsTagNamespace(comptime A: type, comptime B: type) type {
    return struct {
        const info_a: std.builtin.TypeInfo.Enum = @typeInfo(A).Enum;
        const info_b: std.builtin.TypeInfo.Enum = @typeInfo(B).Enum;

        const TagA = info_a.tag_type;
        const TagB = info_b.tag_type;

        const bits_a = @typeInfo(TagA).Int.bits;
        const bits_b = @typeInfo(TagB).Int.bits;

        const UnsignedA = std.meta.Int(.unsigned, bits_a);
        const UnsignedB = std.meta.Int(.unsigned, bits_b);

        const TagCombined = std.meta.Int(.unsigned, bits_a + bits_b);

        const values_a: *const [info_a.fields.len]A = std.enums.values(A)[0..info_a.fields.len];
        const values_b: *const [info_b.fields.len]B = std.enums.values(B)[0..info_b.fields.len];

        const asc_values_a: *const [info_a.fields.len]A = asc_values_a: {
            var asc_values: [info_a.fields.len]A = values_a.*;
            @setEvalBranchQuota(std.math.min(std.math.maxInt(u32), 1000 + asc_values.len));

            std.sort.sort(A, &asc_values, void{}, struct {
                fn lessThan(context: void, lhs: A, rhs: A) bool {
                    _ = context;
                    return @enumToInt(lhs) < @enumToInt(rhs);
                }
            }.lessThan);

            break :asc_values_a &asc_values;
        };
        const asc_values_b: *const [info_b.fields.len]B = asc_values_b: {
            var asc_values: [info_b.fields.len]B = values_b.*;
            @setEvalBranchQuota(std.math.min(std.math.maxInt(u32), 1000 + asc_values.len));

            std.sort.sort(B, &asc_values, void{}, struct {
                fn lessThan(context: void, lhs: B, rhs: B) bool {
                    _ = context;
                    return @enumToInt(lhs) < @enumToInt(rhs);
                }
            }.lessThan);

            break :asc_values_b &asc_values;
        };

        const min_a = asc_values_a[0];
        const min_b = asc_values_b[0];

        const max_a = asc_values_a[asc_values_a.len - 1];
        const max_b = asc_values_b[asc_values_a.len - 1];

        fn assertValidCombo() void {
            if (comptime values_a.len == 0 or values_b.len == 0) {
                @compileError("Cannot combine enums with zero members.");
            }
        }

        fn unsignedA(a: A) UnsignedA {
            assertValidCombo();
            return @bitCast(UnsignedA, @enumToInt(a));
        }

        fn unsignedB(b: B) UnsignedB {
            assertValidCombo();
            return @bitCast(UnsignedB, @enumToInt(b));
        }

        const Bits = packed struct { a: TagA, b: TagB };

        fn tagCombined(a: A, b: B) TagCombined {
            assertValidCombo();
            return @bitCast(TagCombined, Bits{
                .a = @enumToInt(a) - @enumToInt(min_a),
                .b = @enumToInt(b) - @enumToInt(min_b),
            });
        }

        fn extractTagA(combined: TagCombined) TagA {
            assertValidCombo();
            return @bitCast(Bits, combined).a + @enumToInt(min_a);
        }

        fn extractTagB(combined: TagCombined) TagB {
            assertValidCombo();
            return @bitCast(Bits, combined).b + @enumToInt(min_b);
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

                try std.testing.expectEqual(expected_r, actual_r);
                try std.testing.expectEqual(expected_d, actual_d);

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
