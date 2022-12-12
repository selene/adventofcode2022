# -*- coding: utf-8 -*-
"""
Created on Thu Dec 02 22:16:35 2021

@author: Selene
"""

from collections import deque, namedtuple
import copy
import re
import sys
import time

DEBUG = False


Pos = namedtuple('Pos', ['row', 'col'])
Visit = namedtuple('Visit', ['row', 'col', 'moves'])


DIRS = [
    Pos(1, 0),
    Pos(-1, 0),
    Pos(0, 1),
    Pos(0, -1)
]


def initialize(use_input=False, value=None):
    if use_input:
        f = open('12input.txt')
        input_lines = [l.strip() for l in f.readlines()]
        f.close()
    elif value:
        input_lines = value.split('\n')
    else:
        input_lines = """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi""".split('\n')
    
    return input_lines


def lines_to_map(lines):
    hill_map = []
    start_coords = None
    end_coords = None
    
    
    for ridx, line in enumerate(lines):
        row = []
        for cidx, ch in enumerate(line):
            if ch == 'S':
                start_coords = Pos(ridx, cidx)
                elevation = 0
            elif ch == 'E':
                end_coords = Pos(ridx, cidx)
                elevation = ord('z') - ord('a')
            else:
                elevation = ord(ch) - ord('a')
            
            row.append(elevation)
        hill_map.append(row)
    
    return hill_map, start_coords, end_coords
                
                
def print_path(path):
    pass
    # TODO:
    # Find max and min row and col reached
    # Print grid


def neighbors(pos, hill_map, moves):
    result = []
    curr_elevation = hill_map[pos.row][pos.col]
    height = len(hill_map)
    width = len(hill_map[0])
    
    for d in DIRS:
        new_pos = Visit(pos.row + d.row, pos.col + d.col, moves + 1)
        if (
            new_pos.row < 0 or new_pos.row >= height
            or new_pos.col < 0 or new_pos.col >= width
        ):
            continue
        
        new_elevation = hill_map[new_pos.row][new_pos.col]
        if new_elevation - curr_elevation > 1:
            continue
        
        result.append(new_pos)
    
    return result
    

 


def part1(use_input=False, value=None):
    hill_map, start_coords, end_coords = lines_to_map(initialize(use_input, value))
    
    
    search_q = deque(neighbors(start_coords, hill_map, 0))
    searched = {}
    while len(search_q) > 0:
        curr = search_q.popleft()
        
        curr_pos = Pos(curr.row, curr.col)
        if (curr_pos not in searched) or (searched[curr_pos] > curr.moves):
            searched[curr_pos] = curr.moves
            for n in neighbors(curr_pos, hill_map, curr.moves):
                search_q.append(n)
        
    
    end_path_length = searched[end_coords]
    print(f'Shortest path has length {end_path_length}')
    return end_path_length



def neighbors2(pos, hill_map, moves):
    """
    Backwards of neighbors, assuming we're climbing in reverse

    Parameters
    ----------
    pos : TYPE
        DESCRIPTION.
    hill_map : TYPE
        DESCRIPTION.
    moves : TYPE
        DESCRIPTION.

    Returns
    -------
    result : TYPE
        DESCRIPTION.

    """
    result = []
    curr_elevation = hill_map[pos.row][pos.col]
    height = len(hill_map)
    width = len(hill_map[0])
    
    for d in DIRS:
        new_pos = Visit(pos.row + d.row, pos.col + d.col, moves + 1)
        if (
            new_pos.row < 0 or new_pos.row >= height
            or new_pos.col < 0 or new_pos.col >= width
        ):
            continue
        
        new_elevation = hill_map[new_pos.row][new_pos.col]
        if new_elevation - curr_elevation < -1:
            continue
        
        result.append(new_pos)
    
    return result
    
def part2(use_input=False, value=None):
    hill_map, _, end_coords = lines_to_map(initialize(use_input, value))
    
    start_coords = []
    for ridx, row in enumerate(hill_map):
        for cidx, elevation in enumerate(row):
            if elevation == 0:
                start_coords.append((Pos(ridx, cidx)))
    
    
    search_q = deque(neighbors2(end_coords, hill_map, 0))
    searched = {}
    while len(search_q) > 0:
        curr = search_q.popleft()
        
        curr_pos = Pos(curr.row, curr.col)
        if (curr_pos not in searched) or (searched[curr_pos] > curr.moves):
            searched[curr_pos] = curr.moves
            for n in neighbors2(curr_pos, hill_map, curr.moves):
                search_q.append(n)
    
    lowest = 999999
    print(searched)
    for coord in start_coords:
        if coord in searched and searched[coord] < lowest:
            lowest = searched[coord]
    
    print(f'Shortest path has length {lowest}')
    return lowest