const std = @import("std");

pub const FlattenedEnumUnionOptions = struct { name_separator: []const u8 = "_" };
pub fn FlattenEnumUnion(
    comptime EnumUnion: type,
    comptime options: FlattenedEnumUnionOptions,
) type {
    return FlattenEnumUnionImpl(EnumUnion, options.name_separator.len, options.name_separator[0..options.name_separator.len].*);
}

fn FlattenEnumUnionImpl(
    comptime EnumUnion: type,
    comptime name_separator_len: comptime_int,
    comptime name_separator: [name_separator_len]u8,
) type {
    @setEvalBranchQuota(1000 + 1000 * std.meta.fields(EnumUnion).len);
    var new_fields: []const std.builtin.TypeInfo.EnumField = &.{};

    var field_num: comptime_int = 0;
    for (@as([]const std.builtin.TypeInfo.UnionField, @typeInfo(EnumUnion).Union.fields)) |field_info| {
        switch (@typeInfo(field_info.field_type)) {
            .Void => {
                new_fields = new_fields ++ [_]std.builtin.TypeInfo.EnumField{.{
                    .name = field_info.name,
                    .value = field_num,
                }};
                field_num += 1;
            },
            .Enum, .Union => {
                const enum_fields: []const std.builtin.TypeInfo.EnumField = switch (@typeInfo(field_info.field_type)) {
                    .Enum => std.meta.fields(field_info.field_type),
                    .Union => std.meta.fields(FlattenEnumUnionImpl(field_info.field_type, name_separator_len, name_separator)),
                    else => unreachable,
                };

                @setEvalBranchQuota(enum_fields.len);
                for (enum_fields) |enum_field_info| {
                    new_fields = new_fields ++ [_]std.builtin.TypeInfo.EnumField{.{
                        .name = field_info.name ++ name_separator ++ enum_field_info.name,
                        .value = field_num,
                    }};
                    field_num += 1;
                }
            },
            else => unreachable,
        }
    }

    return @Type(@unionInit(std.builtin.TypeInfo, "Enum", std.builtin.TypeInfo.Enum{
        .layout = .Auto,
        .tag_type = std.meta.Int(.unsigned, new_fields.len),
        .fields = new_fields,
        .decls = &.{},
        .is_exhaustive = true,
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
    try std.testing.expectEqual(@as(usize, 4), values.len);
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
}

pub const CombineEnumOptions = struct { name_separator: []const u8 = "_" };
pub fn CombineEnums(
    comptime A: type,
    comptime B: type,
    comptime options: CombineEnumOptions,
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

    return FlattenEnumUnion(NonFlatEnumUnion, .{ .name_separator = options.name_separator });
}

test "CombineEnums Demo 1" {
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
    const DirectionPlusRotation = CombineEnums(Direction, Rotation, .{});
    try std.testing.expectEqual(DirectionPlusRotation, CombineEnums(Direction, Rotation, .{}));

    const values = std.enums.values(DirectionPlusRotation);
    try std.testing.expectEqualStrings("north_clockwise", @tagName(values[0]));
    try std.testing.expectEqualStrings("north_anticlockwise", @tagName(values[1]));
    try std.testing.expectEqualStrings("east_clockwise", @tagName(values[2]));
    try std.testing.expectEqualStrings("east_anticlockwise", @tagName(values[3]));
    try std.testing.expectEqualStrings("south_clockwise", @tagName(values[4]));
    try std.testing.expectEqualStrings("south_anticlockwise", @tagName(values[5]));
    try std.testing.expectEqualStrings("west_clockwise", @tagName(values[6]));
    try std.testing.expectEqualStrings("west_anticlockwise", @tagName(values[7]));
}