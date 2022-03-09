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

/// Expects a tagged union type, whose fields are all either void, an enum, or a union which meets the former
/// two constraints, or the third of these constraints, recursively. The resulting type with be an enum,
/// whose fields are all flattened versions of the possible state combinations.
///
/// So given an input type, with field written as
/// ```
/// monster: enum { slime, goblin, troll },
/// ```
/// using default configuration, the resulting type would include fields named: 
/// * `monster_slime`
/// * `monster_goblin`
/// * `monster_troll`
///
/// This also then applies recursively to fields which are union types,
/// so given an input type, with a field written as
/// ```
/// human: union(enum) { royal, citizen: Citizen },
/// const Citizen = enum { peasant, artisan };
/// ```
/// using default configuration, the resulting type would include fields named:
/// * `human_royal`
/// * `human_citizen_peasant`
/// * `human_citizen_artisan`
pub fn FlattenedEnumUnion(comptime EnumUnion: type) type {
    const name_separator = "_".*;

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
                    .Union => std.meta.fields(FlattenedEnumUnion(field_info.field_type)),
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

    return @Type(@unionInit(std.builtin.TypeInfo, "Enum", std.builtin.TypeInfo.Enum{
        .layout = .Auto,
        .tag_type = std.meta.Int(.unsigned, new_fields.len),
        .fields = new_fields,
        .decls = &.{},
        .is_exhaustive = true,
    }));
}

pub fn flattenEnumUnion(enum_union: anytype) FlattenedEnumUnion(@TypeOf(enum_union)) {
    const name_separator = "_".*;

    const EnumUnion = @TypeOf(enum_union);
    const Flattened = FlattenedEnumUnion(EnumUnion);

    var result: Flattened = undefined;
    inline for (comptime std.enums.values(std.meta.Tag(EnumUnion))) |tag| {
        if (enum_union == tag) {
            const tag_name = @tagName(tag);
            const field_value = @field(enum_union, tag_name);
            const FieldType = @TypeOf(field_value);
            switch (@typeInfo(FieldType)) {
                .Void => return @field(Flattened, @tagName(tag)),
                .Enum => {
                    const lut = comptime lut: {
                        var lut = std.EnumArray(FieldType, Flattened).initUndefined();
                        for (std.enums.values(FieldType)) |subtag| {
                            lut.set(subtag, @field(Flattened, @tagName(tag) ++ name_separator ++ @tagName(subtag)));
                        }
                        break :lut lut;
                    };
                    result = lut.get(field_value);
                },
                .Union => {
                    const flattened = flattenEnumUnion(field_value);
                    inline for (comptime std.enums.values(FlattenedEnumUnion(FieldType))) |subtag| {
                        if (flattened == subtag) {
                            result = @field(Flattened, @tagName(tag) ++ name_separator ++ @tagName(subtag));
                        }
                    }
                },
                else => unreachable,
            }
        }
    }

    return result;
}

pub fn unflattenEnumUnion(comptime EnumUnion: type, flattened: FlattenedEnumUnion(EnumUnion)) EnumUnion {
    const Flattened = FlattenedEnumUnion(EnumUnion);
    const Lut = std.EnumArray(Flattened, EnumUnion);
    const lut: Lut = comptime lut: {
        var lut = Lut.initUndefined();
        for (everyEnumUnionPermutation(EnumUnion)) |permutation| {
            lut.set(flattenEnumUnionPermutation(permutation), permutation);
        }
        break :lut lut;
    };
    return lut.get(flattened);
}

fn everyEnumUnionPermutation(comptime EnumUnion: type) *const [std.meta.fields(FlattenedEnumUnion(EnumUnion)).len]EnumUnion {
    comptime {
        var result: []const EnumUnion = &.{};
        for (@as([]const std.builtin.TypeInfo.UnionField, std.meta.fields(EnumUnion))) |base_field| {
            switch (@typeInfo(base_field.field_type)) {
                .Void => {
                    result = result ++ [_]EnumUnion{@unionInit(EnumUnion, base_field.name, void{})};
                },
                .Enum => for (std.enums.values(base_field.field_type)) |base_field_value| {
                    result = result ++ [_]EnumUnion{@unionInit(EnumUnion, base_field.name, base_field_value)};
                },
                .Union => for (everyEnumUnionPermutation(base_field.field_type)) |base_field_value| {
                    result = result ++ [_]EnumUnion{@unionInit(EnumUnion, base_field.name, base_field_value)};
                },
                else => unreachable,
            }
        }
        return result[0..result.len];
    }
}

fn flattenEnumUnionPermutation(comptime enum_union: anytype) FlattenedEnumUnion(@TypeOf(enum_union)) {
    const name_separator = "_".*;

    const EnumUnion = @TypeOf(enum_union);
    const Flattened = FlattenedEnumUnion(EnumUnion);

    const field_value = @field(enum_union, @tagName(enum_union));
    const FieldType = @TypeOf(field_value);
    return switch (@typeInfo(FieldType)) {
        .Void => @field(Flattened, @tagName(enum_union)),
        .Enum => @field(Flattened, @tagName(enum_union) ++ name_separator ++ @tagName(field_value)),
        .Union => @field(Flattened, @tagName(enum_union) ++ name_separator ++ @tagName(flattenEnumUnionPermutation(field_value))),
        else => unreachable,
    };
}

export fn foo() void {
    var ent = if (std.rand.DefaultPrng.init(0).random().boolean()) Entity{ .human = .{ .guard = .patrol } } else Entity{ .human = .royalty };
    std.mem.doNotOptimizeAway(@call(.{ .modifier = .never_inline }, flattenEnumUnion, .{ent}));
}

const Entity = union(enum) {
    alien,
    monster: Monster,
    human: Human,

    const Monster = enum {
        slime,
        goblin,
        troll,
    };

    const Human = union(enum) {
        royalty,
        peasant,
        guard: Guard,

        const Guard = enum { gate, patrol };
    };
};

test {
    _ = Entity;

    std.debug.print("\n", .{});
    _ = flattenEnumUnion(Entity{ .human = .{ .guard = .patrol } });
    // try std.testing.expectEqual(flattenEnumUnion(Entity{ .human = .{ .guard = .patrol } }), .human_guard_patrol);
}

pub fn combinedEnumsFieldCount(comptime A: type, comptime B: type) comptime_int {
    return @typeInfo(A).Enum.fields.len * @typeInfo(B).Enum.fields.len;
}

pub fn CombinedEnums(comptime A: type, comptime B: type) type {
    const name_separator = "_";
    var new_fields: []const std.builtin.TypeInfo.EnumField = &.{};

    for (@typeInfo(A).Enum.fields) |field_info_a| {
        for (@typeInfo(B).Enum.fields) |field_info_b| {
            new_fields = new_fields ++ [_]std.builtin.TypeInfo.EnumField{.{
                .name = field_info_a.name ++ name_separator ++ field_info_b.name,
                .value = new_fields.len,
            }};
        }
    }

    return @Type(@unionInit(std.builtin.TypeInfo, "Enum", std.builtin.TypeInfo.Enum{
        .layout = .Auto,
        .tag_type = std.meta.Int(.unsigned, new_fields.len - 1),
        .fields = new_fields,
        .decls = &.{},
        .is_exhaustive = true,
    }));
}

pub fn combineEnums(a: anytype, b: anytype) CombinedEnums(@TypeOf(a), @TypeOf(b)) {
    return combineEnumsTemplate(@TypeOf(a), @TypeOf(b))(a, b);
}

pub fn combineEnumsTemplate(comptime A: type, comptime B: type) (fn (A, B) CombinedEnums(A, B)) {
    const name_separator = "_";

    const Combined = CombinedEnums(A, B);
    const Lut = std.EnumArray(A, std.EnumArray(B, Combined));

    const lut: Lut = comptime lut: {
        var lut = Lut.initUndefined();
        for (std.enums.values(A)) |tag_a| {
            for (std.enums.values(B)) |tag_b| {
                const combined_tag = @field(Combined, @tagName(tag_a) ++ name_separator ++ @tagName(tag_b));
                lut.getPtr(tag_a).set(tag_b, combined_tag);
            }
        }
        break :lut lut;
    };

    return comptime struct {
        fn combineEnums(a: A, b: B) Combined {
            return lut.get(a).get(b);
        }
    }.combineEnums;
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
    const name_separator = "_";

    const ReturnType = switch (split) {
        .a => A,
        .b => B,
        .both => std.meta.ArgsTuple(@TypeOf(combineEnumsTemplate(A, B))),
    };

    const Combined = CombinedEnums(A, B);
    const Lut = std.EnumArray(Combined, ReturnType);

    const lut: Lut = comptime lut: {
        var lut = Lut.initUndefined();
        for (std.enums.values(A)) |tag_a| {
            for (std.enums.values(B)) |tag_b| {
                const combined_tag = @field(Combined, @tagName(tag_a) ++ name_separator ++ @tagName(tag_b));
                lut.set(combined_tag, switch (split) {
                    .a => tag_a,
                    .b => tag_b,
                    .both => .{ tag_a, tag_b },
                });
            }
        }
        break :lut lut;
    };

    return comptime struct {
        fn splitCombinedEnum(combined: CombinedEnums(A, B)) ReturnType {
            return lut.get(combined);
        }
    }.splitCombinedEnum;
}

test "combineEnums" {
    const Foo = enum { foo };
    const Bar = enum { bar };
    const combineFooBar = combineEnumsTemplate(Foo, Bar);
    const splitFooBar = splitCombinedEnumTemplate(Foo, Bar, .both);

    const getFoo = splitCombinedEnumTemplate(Foo, Bar, .a);
    const getBar = splitCombinedEnumTemplate(Foo, Bar, .b);

    try std.testing.expectEqual(combineFooBar(.foo, .bar), @call(.{}, combineEnums, splitFooBar(combineFooBar(.foo, .bar))));
    try std.testing.expectEqual(combineFooBar(.foo, .bar), combineFooBar(getFoo(combineFooBar(.foo, .bar)), getBar(combineFooBar(.foo, .bar))));
    try std.testing.expectEqual(Foo.foo, getFoo(combineFooBar(.foo, .bar)));
    try std.testing.expectEqual(Bar.bar, getBar(combineFooBar(.foo, .bar)));

    // const Fizz = enum { fizz };
    // const Buzz = enum { buzz };
    // const combineFizzBuzz = combineEnumsTemplate(Fizz, Buzz, .{ .optionality = .all_optional });
    // try std.testing.expectEqualStrings("fizz", @tagName(combineFizzBuzz(.fizz, null).?));
    // try std.testing.expectEqualStrings("fizz_buzz", @tagName(combineFizzBuzz(.fizz, .buzz).?));
    // try std.testing.expectEqualStrings("buzz", @tagName(combineFizzBuzz(null, .buzz).?));
    // try std.testing.expectEqual(@as(?CombinedEnums(Fizz, Buzz, .{ .optionality = .all_optional }), null), combineFizzBuzz(null, null));
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
