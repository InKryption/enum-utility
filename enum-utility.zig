const std = @import("std");

pub fn flattenedEnumUnionFieldCount(comptime EnumUnion: type) comptime_int {
    comptime {
        var field_num: comptime_int = 0;
        for (@typeInfo(EnumUnion).Union.fields) |field_info| {
            switch (@typeInfo(field_info.field_type)) {
                .Void => field_num += 1,
                .Enum => field_num += std.meta.fields(field_info.field_type).len,
                .Union => field_num += flattenedEnumUnionFieldCount(field_info.field_type),
                else => unreachable,
            }
        }
        return field_num;
    }
}

pub const FlattenedEnumUnionOptions = struct {
    /// if null, flattened enum will take on the minimum necessary integer tag type
    tag_type: ?type = null,
    /// whether the resulting flattened enum should be exhaustive or not
    is_exhaustive: bool = true,
    /// will be used to separate enum field names
    name_separator: []const u8 = "_",
};

pub fn FlattenEnumUnion(
    comptime EnumUnion: type,
    comptime options: FlattenedEnumUnionOptions,
) type {
    return FlattenEnumUnionImpl(
        EnumUnion,
        options.name_separator[0..options.name_separator.len].*,
        options.tag_type,
        options.is_exhaustive,
    );
}

fn FlattenEnumUnionImpl(
    comptime EnumUnion: type,
    comptime name_separator: anytype,
    comptime tag_type: ?type,
    comptime is_exhaustive: bool,
) type {
    std.debug.assert(std.meta.trait.is(.Array)(@TypeOf(name_separator)));
    std.debug.assert(std.meta.Child(@TypeOf(name_separator)) == u8);

    @setEvalBranchQuota(1000 + 1000 * std.meta.fields(EnumUnion).len);
    var new_fields: []const std.builtin.TypeInfo.EnumField = &.{};

    for (@typeInfo(EnumUnion).Union.fields) |field_info| {
        switch (@typeInfo(field_info.field_type)) {
            .Void => {
                new_fields = new_fields ++ [_]std.builtin.TypeInfo.EnumField{.{
                    .name = field_info.name,
                    .value = new_fields.len,
                }};
            },
            .Enum, .Union => {
                const enum_fields: []const std.builtin.TypeInfo.EnumField = switch (@typeInfo(field_info.field_type)) {
                    .Enum => std.meta.fields(field_info.field_type),
                    .Union => std.meta.fields(FlattenEnumUnionImpl(
                        field_info.field_type,
                        name_separator,
                        tag_type,
                        is_exhaustive,
                    )),
                    else => unreachable,
                };

                @setEvalBranchQuota(enum_fields.len);
                for (enum_fields) |enum_field_info| {
                    new_fields = new_fields ++ [_]std.builtin.TypeInfo.EnumField{.{
                        .name = field_info.name ++ name_separator ++ enum_field_info.name,
                        .value = new_fields.len,
                    }};
                }
            },
            else => unreachable,
        }
    }

    const selected_tag_type = tag_type orelse std.math.IntFittingRange(0, new_fields.len);
    return @Type(@unionInit(std.builtin.TypeInfo, "Enum", std.builtin.TypeInfo.Enum{
        .layout = .Auto,
        .tag_type = selected_tag_type,
        .fields = new_fields,
        .decls = &.{},
        .is_exhaustive = is_exhaustive,
    }));
}

test "FlattenEnumUnion Demo 1" {
    const Abcd = union(enum) {
        a: union(enum) { b: union(enum) { c: union(enum) { d } } },
        d: union(enum) { a: union(enum) { b: union(enum) { c } } },
        c: union(enum) { d: union(enum) { a: union(enum) { b } } },
        b: union(enum) { c: union(enum) { d: union(enum) { a } } },
    };
    const Flattened = FlattenEnumUnion(Abcd, .{});
    try std.testing.expectEqual(Flattened, FlattenEnumUnion(Abcd, .{}));

    const values = std.enums.values(Flattened);
    try std.testing.expectEqualStrings("a_b_c_d", @tagName(values[0]));
    try std.testing.expectEqualStrings("d_a_b_c", @tagName(values[1]));
    try std.testing.expectEqualStrings("c_d_a_b", @tagName(values[2]));
    try std.testing.expectEqualStrings("b_c_d_a", @tagName(values[3]));
    try std.testing.expectEqual(@as(usize, flattenedEnumUnionFieldCount(Abcd)), values.len);
}

test "FlattenEnumUnion Demo 2" {
    const Entity = union(enum) {
        human: union(enum) {
            villager: union(enum) {
                farmer,
                baker,
                fisher,
                smith,
            },
            guard: union(enum) {
                gate,
                patrol,
                castle,
            },
        },
        monster: enum {
            goblin,
            ogre,
            dragon,
        },
    };
    const Flattened = FlattenEnumUnion(Entity, .{});
    try std.testing.expectEqual(Flattened, FlattenEnumUnion(Entity, .{}));

    const values = std.enums.values(Flattened);
    try std.testing.expectEqualStrings("human_villager_farmer", @tagName(values[0]));
    try std.testing.expectEqualStrings("human_villager_baker", @tagName(values[1]));
    try std.testing.expectEqualStrings("human_villager_fisher", @tagName(values[2]));
    try std.testing.expectEqualStrings("human_villager_smith", @tagName(values[3]));
    try std.testing.expectEqualStrings("human_guard_gate", @tagName(values[4]));
    try std.testing.expectEqualStrings("human_guard_patrol", @tagName(values[5]));
    try std.testing.expectEqualStrings("human_guard_castle", @tagName(values[6]));
    try std.testing.expectEqualStrings("monster_goblin", @tagName(values[7]));
    try std.testing.expectEqualStrings("monster_ogre", @tagName(values[8]));
    try std.testing.expectEqualStrings("monster_dragon", @tagName(values[9]));
    try std.testing.expectEqual(@as(usize, flattenedEnumUnionFieldCount(Entity)), values.len);
}

pub fn combinedEnumsFieldCount(comptime A: type, comptime B: type, comptime options: CombinedEnumsOptions) comptime_int {
    return @typeInfo(A).Enum.fields.len * @typeInfo(B).Enum.fields.len + switch (options.optionality) {
        .all_obligatory => 0,
        .left_optional => @typeInfo(B).Enum.fields.len,
        .right_optional => @typeInfo(A).Enum.fields.len,
        .all_optional => @typeInfo(A).Enum.fields.len + @typeInfo(B).Enum.fields.len,
    };
}

pub const CombinedEnumsOptions = struct {
    /// if null, flattened enum will take on the minimum necessary integer tag type
    tag_type: ?type = null,
    /// whether the resulting flattened enum should be exhaustive or not
    is_exhaustive: bool = true,
    /// will be used to separate enum field names
    name_separator: []const u8 = "_",
    /// determines the 'optionality' of the enum combo
    optionality: Optionality = .all_obligatory,

    const Optionality = enum {
        /// every member will contain fields from both enums
        all_obligatory,
        /// every member will contain a field from the left-hand-side enum, and optionally from the right-hand-side
        right_optional,
        /// every member will contain a field from the right-hand-side enum, and optionally from the left-hand-side
        left_optional,
        /// every member will contain either a field from the left-hand-side enum, or the right-hand-side enum, or both
        all_optional,
    };
};

pub fn CombinedEnums(
    comptime A: type,
    comptime B: type,
    comptime options: CombinedEnumsOptions,
) type {
    return CombinedEnumsImpl(
        A,
        B,
        options.name_separator[0..options.name_separator.len].*,
        options.tag_type,
        options.is_exhaustive,
        options.optionality,
    );
}

fn CombinedEnumsImpl(
    comptime A: type,
    comptime B: type,
    comptime name_separator: anytype,
    comptime tag_type: ?type,
    comptime is_exhaustive: bool,
    comptime optionality: CombinedEnumsOptions.Optionality,
) type {
    var new_fields: []const std.builtin.TypeInfo.EnumField = &.{};

    for (@typeInfo(A).Enum.fields) |field_info_a| {
        switch (optionality) {
            .all_obligatory, .left_optional => {},
            .all_optional, .right_optional => {
                new_fields = new_fields ++ [_]std.builtin.TypeInfo.EnumField{.{
                    .name = field_info_a.name,
                    .value = new_fields.len,
                }};
            },
        }
        for (@typeInfo(B).Enum.fields) |field_info_b| {
            new_fields = new_fields ++ [_]std.builtin.TypeInfo.EnumField{.{
                .name = field_info_a.name ++ name_separator ++ field_info_b.name,
                .value = new_fields.len,
            }};
        }
    }
    switch (optionality) {
        .all_obligatory, .right_optional => {},
        .all_optional, .left_optional => {
            for (@typeInfo(B).Enum.fields) |field_info_b| {
                new_fields = new_fields ++ [_]std.builtin.TypeInfo.EnumField{.{
                    .name = field_info_b.name,
                    .value = new_fields.len,
                }};
            }
        },
    }

    const selected_tag_type = tag_type orelse std.math.IntFittingRange(0, new_fields.len);
    return @Type(@unionInit(std.builtin.TypeInfo, "Enum", std.builtin.TypeInfo.Enum{
        .layout = .Auto,
        .tag_type = selected_tag_type,
        .fields = new_fields,
        .decls = &.{},
        .is_exhaustive = is_exhaustive,
    }));
}

/// Returns a function that can combine the two enums given, with the given configuration.
pub fn combineEnumsFnTemplate(
    comptime A: type,
    comptime B: type,
    comptime options: CombinedEnumsOptions,
) return_type: {
    const Left: type = switch (options.optionality) {
        .all_obligatory, .right_optional => A,
        .all_optional, .left_optional => ?A,
    };
    const Right: type = switch (options.optionality) {
        .all_obligatory, .left_optional => B,
        .all_optional, .right_optional => ?B,
    };
    break :return_type fn (Left, Right) CombinedEnums(A, B, options);
} {
    const Left: type = switch (options.optionality) {
        .all_obligatory, .right_optional => A,
        .all_optional, .left_optional => ?A,
    };
    const Right: type = switch (options.optionality) {
        .all_obligatory, .left_optional => B,
        .all_optional, .right_optional => ?B,
    };
    const Combined: type = CombinedEnums(A, B, options);
    const Return: type = switch (options.optionality) {
        .left_optional,
        .right_optional,
        .all_obligatory,
        => Combined,
        .all_optional => ?Combined,
    };
    return struct {
        fn combineEnums(a: Left, b: Right) Return {
            const values_a = comptime std.enums.values(A);
            const values_b = comptime std.enums.values(B);

            switch (options.optionality) {
                .all_obligatory => {
                    inline for (values_a) |possible_a| {
                        inline for (values_b) |possible_b| {
                            if (possible_a == a and possible_b == b) {
                                const tag_name = @tagName(possible_a) ++ options.name_separator ++ @tagName(possible_b);
                                return @field(Combined, tag_name);
                            }
                        }
                    }
                },
                .left_optional => blk: {
                    if (a) |_| break :blk;
                    inline for (values_b) |possible_b| {
                        if (possible_b == b) {
                            return @field(Combined, @tagName(possible_b));
                        }
                    }
                },
                .right_optional => blk: {
                    if (b) |_| break :blk;
                    inline for (values_b) |possible_a| {
                        if (possible_a == a) {
                            return @field(Combined, @tagName(possible_a));
                        }
                    }
                },
                .all_optional => blk: {
                    if (a) |_| if (b) |_| break :blk;
                    return null;
                },
            }
        }
    }.combineEnums;
}

test "CombinedEnums" {
    const Rotation = enum {
        clockwise,
        anticlockwise,
    };
    const Direction = enum {
        north,
        east,
        south,
        west,
    };
    const DirectionPlusRotation = CombinedEnums(Direction, Rotation, .{});
    try std.testing.expectEqual(DirectionPlusRotation, CombinedEnums(Direction, Rotation, .{}));

    const values = std.enums.values(DirectionPlusRotation);
    try std.testing.expectEqualStrings("north_clockwise", @tagName(values[0]));
    try std.testing.expectEqualStrings("north_anticlockwise", @tagName(values[1]));
    try std.testing.expectEqualStrings("east_clockwise", @tagName(values[2]));
    try std.testing.expectEqualStrings("east_anticlockwise", @tagName(values[3]));
    try std.testing.expectEqualStrings("south_clockwise", @tagName(values[4]));
    try std.testing.expectEqualStrings("south_anticlockwise", @tagName(values[5]));
    try std.testing.expectEqualStrings("west_clockwise", @tagName(values[6]));
    try std.testing.expectEqualStrings("west_anticlockwise", @tagName(values[7]));
    try std.testing.expectEqual(@as(usize, combinedEnumsFieldCount(Direction, Rotation, .{})), values.len);

    const Fizz = enum { fizz };
    const Buzz = enum { buzz };

    const FizzBuzz1 = CombinedEnums(Fizz, Buzz, .{});
    try std.testing.expect(FizzBuzz1 != CombinedEnums(enum { fizz }, enum { buzz }, .{}));
    try std.testing.expectEqual(FizzBuzz1, CombinedEnums(Fizz, Buzz, .{}));
}

test "CombinedEnums Optionals" {
    const Fizz = enum { fizz };
    const Buzz = enum { buzz };
    const FizzBuzz = CombinedEnums(Fizz, Buzz, .{ .optionality = .all_optional });

    const values = std.enums.values(FizzBuzz);
    try std.testing.expectEqualStrings("fizz", @tagName(values[0]));
    try std.testing.expectEqualStrings("fizz_buzz", @tagName(values[1]));
    try std.testing.expectEqualStrings("buzz", @tagName(values[2]));
    try std.testing.expectEqual(@as(usize, combinedEnumsFieldCount(Fizz, Buzz, .{ .optionality = .all_optional })), values.len);
}

test "combineEnums" {
    const Foo = enum { foo };
    const Bar = enum { bar };
    try std.testing.expectEqualStrings("foo_bar", @tagName(combineEnumsFnTemplate(Foo, Bar, .{})(.foo, .bar)));
    try std.testing.expectEqualStrings("bar_foo", @tagName(combineEnumsFnTemplate(Bar, Foo, .{})(.bar, .foo)));
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

    inline for (comptime std.enums.values(FromType)) |possible_value| {
        if (possible_value == from) {
            const tag_name = @tagName(possible_value);
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
