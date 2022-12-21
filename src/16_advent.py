# -*- coding: utf-8 -*-
"""
Created on Thu Dec 02 22:16:35 2021

@author: Selene
"""

from collections import defaultdict, deque, namedtuple
import copy
from enum import IntEnum
import math
import re
import sys
import time

DEBUG = False

MOVE_OPEN = '**'
MOVE_WAIT = '..'
MAX_PATH_LENGTH = 60
MAX_REVISIT = 5


Move = namedtuple('Move', ['valve', 'path', 'total_flow', 'open_valves', 'score'])


class Valve:
    def __init__(self, name, flow, tunnels):
        self.name = name
        self.flow = flow
        self.tunnels = tunnels
    
    def __repr__(self):
        return f'Valve({self.name}, {self.flow}, {self.tunnels})'



def initialize(use_input=False, value=None):
    if use_input:
        f = open('16input.txt')
        input_lines = [l for l in f.readlines()]
        f.close()
    elif value:
        input_lines = value.split('\n')
    else:
        input_lines = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II""".split('\n')
    
    return [l.strip() for l in input_lines]


def parse_input(lines):
    valves = {}
    rex_valve = r'Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? ([A-Z ,]+)'
    for line in lines:
        match_re = re.match(rex_valve, line)
        name = match_re[1]
        flow = match_re[2]
        tunnels = match_re[3].split(', ')
        valves[name] = Valve(name, int(flow), tunnels)
    
    return valves



def part1(use_input=False, value=None):
    valves = parse_input(initialize(use_input, value))
    valve_names = sorted(valves.keys())
    
    max_flow = sum([valve.flow for valve in valves.values()])
    
    visited = set()
    visited_valves = set()
    queue = deque([Move(
        valves[valve_names[0]],
        '',
        0,
        set(),
        0
    )])
    final_scores = {}


    i = 0
    while len(queue) > 0:
        if i % 10 == 0:
            print(f'\n ==== Iteration {i} ====')
            print(queue)
        i += 1
        
        curr_valve, path, total_flow, open_valves, score = queue.popleft()
        if path in visited:
            # We've been here before
            continue

        last_move = path[-2:]
        if curr_valve.name in visited_valves and last_move != MOVE_WAIT and last_move != MOVE_OPEN:
            continue

        visited.add(path)
        visited_valves.add(curr_valve.name)

        if len(path) == MAX_PATH_LENGTH:
            # Out of time!
            print(f'New final path: {path} = {score}')
            final_scores[path] = score
            continue
        
        if total_flow >= max_flow or last_move == MOVE_WAIT:
            time_left = (MAX_PATH_LENGTH - len(path)) // 2
            final_scores[path] = score + total_flow * time_left
            continue
        
        if curr_valve.flow > 0 and (curr_valve.name not in open_valves):
            queue.append(Move(
                curr_valve,
                path + MOVE_OPEN,
                total_flow + curr_valve.flow,
                open_valves | set([curr_valve.name]),
                score + total_flow + curr_valve.flow
            ))
        
        moves = 0
        for name in curr_valve.tunnels:
            # if name in path and score == 0:
            #     continue
            if path.count(name) > MAX_REVISIT:
                continue
            queue.append(Move(
                valves[name],
                path + name,
                total_flow,
                open_valves,
                score + total_flow
            ))
            moves += 1
        

        queue.append(Move(
            curr_valve,
            path + MOVE_WAIT,
            total_flow,
            open_valves,
            score + total_flow
        ))
    
    for path, value in final_scores.items():
        print(f'{value} - {path}')
    
    print(f'Max is {max(final_scores.values())}')
    return max(final_scores.values())
            
    


def part2(use_input=False, value=None):
    valves = parse_input(initialize(use_input, value))
