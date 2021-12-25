using Printf
using Pkg

Pkg.add("DataStructures"); 
using DataStructures;

include("util.jl")

# read relative to this dir
cd(@__DIR__)

# input = readlines("input/day23.txt")
# input = readlines("input/day23-test2.txt")

# parsed = [line for line in input]

# target = [
#     "#############",
#     "#...........#",
#     "###A#B#C#D###",
#     "  #A#B#C#D#",
#     "  #########",
# ]

initial_state = Dict(
    "hallway" => [".", ".", "-", ".", "-", ".", "-", ".", "-", ".", "."],
    "roomA" => ["C", "B"], "roomB" => ["D", "A"], "roomC" => ["A", "D"], "roomD" => ["B", "C"],
    # TODO testing ??
    "energyUsed" => 0
)

pod_energy = Dict(
    "A" => 1,
    "B" => 10,
    "C" => 100,
    "D" => 1000,
)

## Part 1

function canMove(state, from, to)
    for p in min(from, to):max(from, to)
        # from may be occupied
        if p != from && state["hallway"][p] != "." && state["hallway"][p] != "-"
            return false
        end
    end
    return true
end

function canEnter(state, room, pod)
    if !endswith(room, pod)
        # wrong room
        return false
    end

    room = state[room]
    for i in 1:length(room)
        if room[i] != pod && room[i] != "."
            # other pods are still here
            return false
        end
    end

    return true
end

function enter(room, pod)
    ret = deepcopy(room)
    for i in length(room):-1:1
        if ret[i] == "."
            ret[i] = pod
            return ret
        end
    end
    throw("Room is full")
end

function leave(room)
    ret = deepcopy(room)
    for i in 1:length(room)
        if ret[i] != "."
            ret[i] = "."
            return ret
        end
    end
    throw("There is no one here")
end

function in_room(room)
    count = 0
    for i in 1:length(room)
        if room[i] != "."
            count += 1
        end
    end
    count
end

function winning(state)
    onlyPods(state["roomA"], "A") && 
    onlyPods(state["roomB"], "B") &&
    onlyPods(state["roomC"], "C") &&
    onlyPods(state["roomD"], "D")
end

function onlyPods(room, pod)
    for r in room
        if r != pod
            return false
        end
    end
    return true
end

function nextStates(state, room_size)
    new_states = []

    rooms = [("roomA", 3), ("roomB", 5), ("roomC", 7), ("roomD", 9)]
    hallway_locs = [1,2,4,6,8,10,11]

    for (room_name, room_i) in rooms
        room_state = state[room_name]
        for room_pos in 1:room_size
            leave_cost = room_pos
            pod = room_state[room_pos]
            if pod in ["A", "B", "C", "D"]
                # room to room move
                for (target_name, target_room_i) in rooms
                    if room_i == target_room_i
                        continue
                    end
                    if canEnter(state, target_name, pod) && canMove(state, room_i, target_room_i)
                        enter_cost = room_size - in_room(state[target_name])
                        ns = deepcopy(state)
                        ns[room_name] = leave(ns[room_name])
                        ns[target_name] = enter(ns[target_name], pod)
                        ns["energyUsed"] += (leave_cost + abs(room_i - target_room_i) + enter_cost) * pod_energy[pod]
                        push!(new_states, ns)
                    end
                end

                # room to hallway move
                for target_hallway_i in hallway_locs
                    if canMove(state, room_i, target_hallway_i)
                        ns = deepcopy(state)
                        ns[room_name] = leave(ns[room_name])
                        ns["hallway"][target_hallway_i] = pod
                        ns["energyUsed"] += (leave_cost + abs(room_i - target_hallway_i)) * pod_energy[pod]
                        push!(new_states, ns)
                    end
                end

                # inner pod can't move
                break
            end
        end
    end

    # hallway to room
    for source_hallway_i in hallway_locs
        pod = state["hallway"][source_hallway_i]
        if !(pod in ["A", "B", "C", "D"])
            continue
        end
        for (target_name, target_room_i) in rooms
            if canEnter(state, target_name, pod) && canMove(state, source_hallway_i, target_room_i)
                enter_cost = room_size - in_room(state[target_name])
                ns = deepcopy(state)
                ns["hallway"][source_hallway_i] = "."
                ns[target_name] = enter(ns[target_name], pod)
                ns["energyUsed"] += (abs(source_hallway_i - target_room_i) + enter_cost) * pod_energy[pod]
                push!(new_states, ns)

                # log("into room", ns)
            end
        end
    end

    return new_states
end


function part1()
    q = PriorityQueue()

    prio_push(q, initial_state, initial_state["energyUsed"])

    visited = Set()

    state_count = 0
    for i in 1:1000000
        s = dequeue!(q)
        state_count += 1

        key = [s["hallway"], s["roomA"], s["roomB"], s["roomC"], s["roomD"]]
        if key in visited
            continue
        end
        push!(visited, key)

        # log(i, s)

        if winning(s)
            log("winning!", s)
            println(s["energyUsed"])
            break
        end

        for ns in nextStates(s, 2)
            prio_push(q, ns, ns["energyUsed"])
        end
    end

    log("states processed", state_count)
end
@time part1()

## part 2

v2_initial_state = Dict(
    "hallway" => [".", ".", "-", ".", "-", ".", "-", ".", "-", ".", "."],
    "roomA" => ["C", "D", "D", "B"], "roomB" => ["D", "C", "B", "A"], "roomC" => ["A", "B", "A", "D"], "roomD" => ["B", "A", "C", "C"],
    # "roomA" => ["B", "D", "D", "A"], "roomB" => ["C", "C", "B", "D"], "roomC" => ["B", "B", "A", "C"], "roomD" => ["D", "A", "C", "A"],
    # TODO testing ??
    "energyUsed" => 0
)

function part2()
    q = PriorityQueue()

    prio_push(q, v2_initial_state, initial_state["energyUsed"])

    visited = Set()

    state_count = 0
    for i in 1:10000000
        s = dequeue!(q)
        state_count += 1

        key = [s["hallway"], s["roomA"], s["roomB"], s["roomC"], s["roomD"]]
        if key in visited
            continue
        end
        push!(visited, key)

        # log(i, s)

        if winning(s)
            log("winning!", s)
            println(s["energyUsed"])
            break
        end

        for ns in nextStates(s, 4)
            prio_push(q, ns, ns["energyUsed"])
        end
    end

    log("states processed", state_count)

end
@time part2()
