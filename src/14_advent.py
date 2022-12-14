# -*- coding: utf-8 -*-
"""
Created on Thu Dec 02 22:16:35 2021

@author: Selene
"""

from collections import defaultdict, namedtuple
import math
import re
import sys
import time

DEBUG = False

class Tile:
    AIR = ' '
    ROCK = '█'
    SAND = 'A'
    SOURCE = '+'
    FLOOR = '═'


Pos = namedtuple('Pos', ['row', 'col'])
Range = namedtuple('Range', ['min', 'max'])


SAND_SOURCE_POS = Pos(0, 500)

FALL_DIRS = [
    Pos(1, 0),  # Straight down
    Pos(1, -1), # Down and to the left
    Pos(1, 1)   # Down and to the right
]


def initialize(use_input=False, value=None):
    if use_input:
        f = open('14input.txt')
        input_lines = [l for l in f.readlines()]
        f.close()
    elif value:
        input_lines = value.split('\n')
    else:
        input_lines = """498,4 -> 498,6 -> 496,6
        503,4 -> 502,4 -> 502,9 -> 494,9""".split('\n')
    
    return [l.strip() for l in input_lines]


def extract_rock_paths(lines):
    paths = []
    for line in lines:
        path = []
        points = line.split(' -> ')
        for point in points:
            coords = point.split(',')
            # Coords are given as x,y which is col,row
            path.append(Pos(int(coords[1]), int(coords[0])))
        paths.append(path)
    
    return paths
            

def cave_bounds(paths):
    min_row = 99999
    min_col = 99999
    max_row = 0
    max_col = 0
    
    
    for path in paths:
        for point in path:
            if point.row < min_row: min_row = point.row
            if point.row > max_row: max_row = point.row
            if point.col < min_col: min_col = point.col
            if point.col > max_col: max_col = point.col
    
    print(f'col: {min_col} - {max_col}')
    print(f'row: {min_row} - {max_row}')

    return Range(Pos(min_row, min_col), Pos(max_row, max_col))
    


def lines_to_map(lines):
    rock_paths = extract_rock_paths(lines)
    bounds = cave_bounds(rock_paths)
    
    cave = defaultdict(lambda: defaultdict(lambda: Tile.AIR))
    cave[SAND_SOURCE_POS[0]][SAND_SOURCE_POS[1]] = Tile.SOURCE
    
    print(f'Paths: {rock_paths}')
    for path in rock_paths:
        curr_point = path[0]
        for point in path[1:]:
            print(f'Drawing path from {curr_point} -> {point}')
            cave[curr_point.row][curr_point.col] = Tile.ROCK
            while curr_point.row != point.row or curr_point.col != point.col:
                row_delta = point.row - curr_point.row
                col_delta = point.col - curr_point.col
                
                prev_point = curr_point
                
                if row_delta == 0:
                    curr_point = Pos(point.row, curr_point.col+ col_delta // abs(col_delta))
                else:
                    curr_point = Pos(curr_point.row + row_delta // abs(row_delta), point.col)
                    
                print(f'  new curr_point = {curr_point}')
                    
                cave[curr_point.row][curr_point.col] = Tile.ROCK
                
                if prev_point == curr_point:
                    print('HALP PREV AND CURR ARE SAME! row_delta={row_delta}, col_delta={col_delta}')
    
    print_map(cave, bounds)
    return cave, bounds
    
    
def print_map(cave, bounds):
    # TODO figure out the columns
    print(f'Columns from {bounds.min.col} - {bounds.max.col}')
    for r in range(0, bounds.max.row + 1):
        print('{:>3}'.format(r), end=' ')
        row_chars = [
            cave[r][c]
            for c in range(bounds.min.col - 1, bounds.max.col + 1)
        ]
        print(''.join(row_chars))
            

def next_pos(cave, pos):
    for d in FALL_DIRS:
        new_pos = Pos(pos.row + d.row, pos.col + d.col)
        if cave[new_pos.row][new_pos.col] == Tile.AIR:
            return new_pos
    return None

def fall_sand(cave, bounds):
    """
    

    Parameters
    ----------
    cave : TYPE
        DESCRIPTION.
    bounds : TYPE
        DESCRIPTION.

        DESCRIPTION.

    Returns
    -------
    None -- Sand falls into abyss
    Pos -- Sand settles in this position

    """
    prev_sand_pos = SAND_SOURCE_POS
    new_sand_pos = next_pos(cave, prev_sand_pos)
    while new_sand_pos:
        if new_sand_pos.row > bounds.max.row:
            # Assume we're spilling into the abyss
            return None
        prev_sand_pos = new_sand_pos
        new_sand_pos = next_pos(cave, new_sand_pos)
    
    return prev_sand_pos
    

def part1(use_input=False, value=None):
    cave, bounds = lines_to_map(initialize(use_input, value))
    
    new_sand_count = 0
    new_sand = fall_sand(cave, bounds)
    while new_sand:
        cave[new_sand.row][new_sand.col] = Tile.SAND
        new_sand_count += 1
        
        new_sand = fall_sand(cave, bounds)
    
    print_map(cave, bounds)
    return new_sand_count
    
    

def part2(use_input=False, value=None):
    cave, rock_bounds = lines_to_map(initialize(use_input, value))
    
    cave[rock_bounds.max.row+2] = defaultdict(lambda: Tile.FLOOR)
    bounds = Range(rock_bounds.min, Pos(rock_bounds.max.row+2, rock_bounds.max.col))
    
    new_sand_count = 0
    new_sand = fall_sand(cave, bounds)
    while new_sand != SAND_SOURCE_POS:
        cave[new_sand.row][new_sand.col] = Tile.SAND
        new_sand_count += 1
        
        new_sand = fall_sand(cave, bounds)
    
    print_map(cave, bounds)
    return new_sand_count + 1
    