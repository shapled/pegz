//! Left recursion detection and transformation
//! Based on pigeon's builder/left_recursion.go

const std = @import("std");
const ast = @import("ast.zig");
const scc = @import("scc.zig");

const Self = @This();

/// Error types for left recursion detection
pub const LeftRecursionError = error{
    NoLeader,
    GrammarHasLeftRecursion,
};

/// Compute nullables for all rules in the grammar
pub fn computeNullables(rules: *std.StringHashMap(*ast.Rule)) void {
    var iter = rules.valueIterator();
    while (iter.next()) |rule_ptr| {
        _ = rule_ptr.*.nullableVisit(rules.*);
    }
}

/// Build the "first graph" - graph of left-invocations
/// There's an edge from A to B if A may invoke B at its initial position
/// Note: this requires nullable flags to have been computed first
pub fn makeFirstGraph(
    allocator: std.mem.Allocator,
    rules: *std.StringHashMap(*ast.Rule),
) !std.StringHashMap(std.StringHashMap(void)) {
    var graph = std.StringHashMap(std.StringHashMap(void)).init(allocator);
    var vertices = std.StringHashMap(void).init(allocator);

    // Build initial graph
    var rule_iter = rules.iterator();
    while (rule_iter.next()) |entry| {
        const rule_name = entry.key_ptr.*;
        const rule = entry.value_ptr.*;

        const names = try rule.initialNames(allocator);
        try graph.put(rule_name, names);

        // Add to vertices
        var name_iter = names.iterator();
        while (name_iter.next()) |name_entry| {
            try vertices.put(name_entry.key_ptr.*, {});
        }
    }

    // Ensure all vertices have entries in the graph
    var vertex_iter = vertices.iterator();
    while (vertex_iter.next()) |entry| {
        const vertex = entry.key_ptr.*;
        if (!graph.contains(vertex)) {
            try graph.put(vertex, std.StringHashMap(void).init(allocator));
        }
    }

    return graph;
}

/// Find a leader in an SCC - a node that all cycles go through
pub fn findLeader(
    allocator: std.mem.Allocator,
    graph: *const std.StringHashMap(std.StringHashMap(void)),
    component: *const std.StringHashMap(void),
) ![]const u8 {
    // Try to find a leader such that all cycles go through it
    var leaders = std.StringHashMap(void).init(allocator);
    defer leaders.deinit();

    // Initialize leaders with all SCC members
    var comp_iter = component.iterator();
    while (comp_iter.next()) |entry| {
        try leaders.put(entry.key_ptr.*, {});
    }

    // For each start node, find cycles and intersect with leaders
    var comp_iter2 = component.iterator();
    while (comp_iter2.next()) |entry| {
        const start = entry.key_ptr.*;
        var cycles = try scc.findCyclesInSCC(allocator, graph, component, start);
        defer {
            for (cycles.items) |*cycle| {
                cycle.deinit(allocator);
            }
            cycles.deinit(allocator);
        }

        // For each cycle, remove nodes not in the cycle from leaders
        for (cycles.items) |cycle| {
            var map_cycle = std.StringHashMap(void).init(allocator);
            defer map_cycle.deinit();

            for (cycle.items) |name| {
                try map_cycle.put(name, {});
            }

            var leader_iter = leaders.iterator();
            while (leader_iter.next()) |leader_entry| {
                const k = leader_entry.key_ptr.*;
                if (!map_cycle.contains(k)) {
                    _ = leaders.remove(k);
                }
            }

            if (leaders.count() == 0) {
                return LeftRecursionError.NoLeader;
            }
        }
    }

    // Pick an arbitrary leader from the candidates
    var leader_iter = leaders.iterator();
    const leader = (leader_iter.next() orelse return LeftRecursionError.NoLeader).key_ptr.*;
    return leader;
}

/// Compute left recursive rules
pub fn computeLeftRecursives(
    allocator: std.mem.Allocator,
    rules: *std.StringHashMap(*ast.Rule),
) !bool {
    const graph = try makeFirstGraph(allocator, rules);

    // Collect vertices
    var vertices_list = try std.ArrayList([]const u8).initCapacity(allocator, 0);
    var graph_iter = graph.iterator();
    while (graph_iter.next()) |entry| {
        try vertices_list.append(allocator, entry.key_ptr.*);
    }

    var sccs = try scc.stronglyConnectedComponents(allocator, vertices_list, &graph);
    defer {
        for (sccs.items) |*component| {
            component.deinit();
        }
        sccs.deinit(allocator);
    }
    defer {
        vertices_list.deinit(allocator);
    }

    var have_left_recursion = false;

    for (sccs.items) |component| {
        if (component.count() > 1) {
            // Multi-node SCC - all rules are left recursive
            var comp_iter = component.iterator();
            while (comp_iter.next()) |entry| {
                const name = entry.key_ptr.*;
                if (rules.get(name)) |rule| {
                    rule.left_recursive = true;
                    have_left_recursion = true;
                }
            }

            // Find a leader
            const leader = try findLeader(allocator, &graph, &component);
            if (rules.get(leader)) |rule| {
                rule.leader = true;
            }
        } else {
            // Single-node SCC
            var name: []const u8 = undefined;
            var comp_iter = component.iterator();
            if (comp_iter.next()) |entry| {
                name = entry.key_ptr.*;
            }

            // Check for self-loop (direct left recursion)
            if (graph.get(name)) |dsts| {
                if (dsts.contains(name)) {
                    if (rules.get(name)) |rule| {
                        rule.left_recursive = true;
                        rule.leader = true;
                        have_left_recursion = true;
                    }
                }
            }
        }
    }

    return have_left_recursion;
}

/// Prepare grammar for left recursion handling
pub fn prepareGrammar(
    allocator: std.mem.Allocator,
    grammar: *ast.Grammar,
) !struct { have_left_recursion: bool, err: ?anyerror } {
    // Build rule map
    var rule_map = std.StringHashMap(*ast.Rule).init(allocator);
    for (grammar.rules.items) |rule| {
        try rule_map.put(rule.name.value, rule);
    }

    computeNullables(&rule_map);

    const have_left_recursion = computeLeftRecursives(allocator, &rule_map) catch |err| {
        return .{ .have_left_recursion = false, .err = err };
    };

    return .{ .have_left_recursion = have_left_recursion, .err = null };
}
