const std = @import("std");
const builtin = @import("builtin");

pub fn combinedEnumsFieldCount(comptime A: type, comptime B: type) comptime_int {
    return @typeInfo(A).Enum.fields.len * @typeInfo(B).Enum.fields.len;
}

pub fn CombinedEnums(comptime A: type, comptime B: type) type {
    comptime {
        const name_separator = "_";
        var new_fields: []const std.builtin.TypeInfo.EnumField = &[_]std.builtin.TypeInfo.EnumField{};

        for (@typeInfo(A).Enum.fields) |field_a| {
            const a = @field(A, field_a.name);
            for (@typeInfo(B).Enum.fields) |field_b| {
                const b = @field(B, field_b.name);

                const tag = if (A == u0 and B == u0) 0 else combinedEnumsTag(a, b);
                new_fields = new_fields ++ [_]std.builtin.TypeInfo.EnumField{.{
                    .name = @tagName(a) ++ name_separator ++ @tagName(b),
                    .value = tag,
                }};
            }
        }

        return @Type(@unionInit(std.builtin.TypeInfo, "Enum", std.builtin.TypeInfo.Enum{
            .layout = .Auto,
            .tag_type = CombinedEnumsTag(A, B),
            .fields = new_fields,
            .decls = &[_]std.builtin.TypeInfo.Declaration{},
            .is_exhaustive = true,
        }));
    }
}

pub fn combineEnums(a: anytype, b: anytype) CombinedEnums(@TypeOf(a), @TypeOf(b)) {
    const Combined = CombinedEnums(@TypeOf(a), @TypeOf(b));
    return @intToEnum(Combined, combinedEnumsTag(a, b));
}

pub fn combineEnumsTemplate(comptime A: type, comptime B: type) (fn (A, B) CombinedEnums(A, B)) {
    return comptime struct {
        fn combineEnumsFn(a: A, b: B) CombinedEnums(A, B) {
            return combineEnums(a, b);
        }
    }.combineEnumsFn;
}

fn CombinedEnumsTag(comptime A: type, comptime B: type) type {
    const TagA = @typeInfo(A).Enum.tag_type;
    const bits_a = @typeInfo(TagA).Int.bits;

    const TagB = @typeInfo(B).Enum.tag_type;
    const bits_b = @typeInfo(TagB).Int.bits;

    return std.meta.Int(.unsigned, bits_a + bits_b);
}

inline fn combinedEnumsTag(a: anytype, b: anytype) CombinedEnumsTag(@TypeOf(a), @TypeOf(b)) {
    const A = @TypeOf(a);
    const B = @TypeOf(b);
    const Combined = CombinedEnumsTag(A, B);

    const TagA = @typeInfo(A).Enum.tag_type;
    const TagB = @typeInfo(B).Enum.tag_type;

    const bits_a = @typeInfo(TagA).Int.bits;
    const bits_b = @typeInfo(TagB).Int.bits;

    const UnsignedA = std.meta.Int(.unsigned, bits_a);
    const UnsignedB = std.meta.Int(.unsigned, bits_b);

    if (comptime bits_a == 0 and bits_b == 0) {
        switch (comptime builtin.zig_backend) {
            .stage1 => @compileError(
                \\Note: A compiler bug makes it impossible to support combining two enums which both have 0 bit
                \\backing integers, so we'll have to wait until stage2 to see if its fixed.
            ),
            .stage2_llvm,
            .stage2_c,
            .stage2_wasm,
            .stage2_arm,
            .stage2_x86_64,
            .stage2_aarch64,
            .stage2_x86,
            .stage2_riscv64,
            => @compileError(
                \\Either remove this code, or change this message to indicate the code still doesn't work
                \\for the given backend.
            ),
            .other => {},
            _ => {},
        }
        return 0;
    }

    if (comptime bits_a == 0 and bits_b != 0) return @bitCast(UnsignedB, @enumToInt(b));
    if (comptime bits_a != 0 and bits_b == 0) return @bitCast(UnsignedA, @enumToInt(a));

    const a_min = comptime @enumToInt(enumMinMax(A).min);
    const b_min = comptime @enumToInt(enumMinMax(B).min);

    const tag_a: Combined = @bitCast(UnsignedA, @enumToInt(a));
    const tag_b: Combined = @bitCast(UnsignedB, @enumToInt(b));

    return ((tag_a - a_min) << bits_b) | (tag_b - b_min);
}

inline fn enumMinMax(comptime Enum: type) struct { min: Enum, max: Enum } {
    const lessThan = comptime struct {
        fn lessThan(context: void, lhs: Enum, rhs: Enum) bool {
            _ = context;
            return std.math.order(@enumToInt(lhs), @enumToInt(rhs)) == .lt;
        }
    }.lessThan;
    const values = comptime std.enums.values(Enum);
    return .{
        .min = values[std.sort.argMin(Enum, values, void{}, lessThan).?],
        .max = values[std.sort.argMax(Enum, values, void{}, lessThan).?],
    };
}

inline fn combinedEnumsATag(comptime A: type, comptime B: type, combined: CombinedEnums(A, B)) std.meta.Tag(A) {
    const TagA = @typeInfo(A).Enum.tag_type;
    const TagB = @typeInfo(B).Enum.tag_type;

    const bits_b = @typeInfo(TagB).Int.bits;
    const a_min = comptime @enumToInt(enumMinMax(A).min);

    const tag = @enumToInt(combined);
    return @truncate(TagA, (tag >> bits_b) + a_min);
}

inline fn combinedEnumsBTag(comptime A: type, comptime B: type, combined: CombinedEnums(A, B)) std.meta.Tag(B) {
    const TagB = @typeInfo(B).Enum.tag_type;

    const b_min = comptime @enumToInt(enumMinMax(B).min);

    const tag = @enumToInt(combined);
    return @truncate(TagB, tag) + b_min;
}

pub const SplitCombinedEnumComponents = enum { a, b, both };
pub fn splitCombinedEnumTemplate(
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

    return comptime struct {
        fn splitCombinedEnum(combined: Combined) ReturnType {
            switch (comptime split) {
                .a => return @intToEnum(A, combinedEnumsATag(A, B, combined)),
                .b => return @intToEnum(B, combinedEnumsBTag(A, B, combined)),
                .both => return .{
                    @intToEnum(A, combinedEnumsATag(A, B, combined)),
                    @intToEnum(B, combinedEnumsBTag(A, B, combined)),
                },
            }
        }
    }.splitCombinedEnum;
}

test "combineEnums" {
    const Direction = enum(u8) { north = 255, east = 33, south = 29, west = 11 };
    const Rotation = enum(u8) { clockwise, anticlockwise };
    const Combined = CombinedEnums(Direction, Rotation);

    const combineDirRot = comptime combineEnumsTemplate(Direction, Rotation);

    const getBoth = comptime splitCombinedEnumTemplate(Direction, Rotation, .both);
    const getDir = comptime splitCombinedEnumTemplate(Direction, Rotation, .a);
    const getRot = comptime splitCombinedEnumTemplate(Direction, Rotation, .b);

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
