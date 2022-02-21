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
    /// will be used to separate enum field names
    name_separator: []const u8 = "_",
    /// if null, flattened enum will take on the minimum necessary integer tag type
    tag_type: ?type = null,
    /// whether the resulting flattened enum should be exhaustive or not
    is_exhaustive: bool = true,
};

pub fn FlattenEnumUnion(
    comptime EnumUnion: type,
    comptime options: FlattenedEnumUnionOptions,
) type {
    return FlattenEnumUnionImpl(EnumUnion, options.name_separator.len, options.name_separator[0..options.name_separator.len].*);
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

    const selected_tag_type = tag_type orelse
        std.meta.Int(.unsigned, if (new_fields.len == 0) 0 else (new_fields.len - 1));

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

pub fn combinedEnumsFieldCount(comptime A: type, comptime B: type) comptime_int {
    return @typeInfo(A).Enum.fields.len * @typeInfo(B).Enum.fields.len;
}

pub const CombinedEnumsOptions = FlattenedEnumUnionOptions;

pub fn CombinedEnums(
    comptime A: type,
    comptime B: type,
    comptime options: CombinedEnumsOptions,
) type {
    var union_fields: []const std.builtin.TypeInfo.UnionField = &.{};

    for (@as([]const std.builtin.TypeInfo.EnumField, @typeInfo(A).Enum.fields)) |field_info_a| {
        union_fields = union_fields ++ &[_]std.builtin.TypeInfo.UnionField{.{
            .name = field_info_a.name,
            .field_type = B,
            .alignment = @alignOf(B),
        }};
    }

    const NonFlatEnumUnion = @Type(@unionInit(std.builtin.TypeInfo, "Union", std.builtin.TypeInfo.Union{
        .layout = .Auto,
        .tag_type = A,
        .fields = union_fields,
        .decls = &.{},
    }));

    return FlattenEnumUnion(NonFlatEnumUnion, options);
}

pub fn combineEnums(a: anytype, b: anytype, comptime options: CombinedEnumsOptions) CombinedEnums(@TypeOf(a), @TypeOf(b), options) {
    const values_a = comptime std.enums.values(@TypeOf(a));
    const values_b = comptime std.enums.values(@TypeOf(b));

    @setEvalBranchQuota(10_000 + 1000 * values_a.len * values_b.len);
    inline for (values_a) |possible_a| {
        if (possible_a == a) {
            inline for (values_b) |possible_b| {
                if (possible_b == b) {
                    const field_name = @tagName(possible_a) ++ options.name_separator ++ @tagName(possible_b);
                    return @field(CombinedEnums(@TypeOf(a), @TypeOf(b), options), field_name);
                }
            }
            unreachable;
        }
    }
    unreachable;
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
    try std.testing.expectEqual(@as(usize, combinedEnumsFieldCount(Direction, Rotation)), values.len);

    const Fizz = enum { fizz };
    const Buzz = enum { buzz };

    const FizzBuzz1 = CombineEnums(Fizz, Buzz, .{});
    const FizzBuzz2 = CombineEnums(enum { fizz }, enum { buzz }, .{});
    try std.testing.expect(FizzBuzz1 != FizzBuzz2);
    try std.testing.expectEqual(FizzBuzz1, CombineEnums(Fizz, Buzz, .{}));
}

test "combineEnums" {
    const Foo = enum { foo };
    const Bar = enum { bar };
    try std.testing.expectEqualStrings("foo_bar", @tagName(combineEnums(Foo.foo, Bar.bar, .{})));
    try std.testing.expectEqualStrings("bar_foo", @tagName(combineEnums(Bar.bar, Foo.foo, .{})));
}
