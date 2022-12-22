# -*- coding: utf-8 -*-
"""
Created on Thu Dec 02 22:16:35 2021

@author: Selene
"""

from collections import defaultdict, namedtuple
from enum import IntEnum
import math
import re
import sys


DEBUG = False


class TileContent:
    AIR = ' '
    WALL = '#'
    FLOOR = '.'


class Tile:
    def __init__(self, row, column, content, neighbors):
        self.row = row
        self.column = column
        self.content = content
        self.neighbors = neighbors or [None] * 4


Pos = namedtuple('Pos', ['row', 'col'])

DIRECTIONS = [
    Pos(0, 1),  # right >
    Pos(1, 0),  # down v
    Pos(0, -1), # left <
    Pos(-1, 0), # up ^
]

class Dir(IntEnum):
    RIGHT = 0
    DOWN = 1
    LEFT = 2
    UP = 3

class MoveType():
    FORWARD = 'F'
    TURN_RIGHT = 'R'
    TURN_LEFT = 'L'

Move = namedtuple('Move', ['type', 'val'])


def initialize(use_input=False, value=None):
    if use_input:
        f = open('22input.txt')
        input_lines = [l for l in f.readlines()]
        f.close()
    elif value:
        input_lines = value.split('\n')
    else:
        input_lines = """        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5""".split('\n')
    
    return [line.replace('\n', '') for line in input_lines]


def parse_input(lines):
    jungle = defaultdict(lambda: defaultdict(lambda: TileContent.AIR))
    moves = []
    
    for ridx, line in enumerate(lines):
        r = ridx + 1
        if '.' in line or '#' in line:
            for cidx, char in enumerate(line):
                if char != '.' and char != '#':
                    continue
                c = cidx + 1
                jungle[r][c] = char
        elif re.match('[0-9LR]+', line):
            curr_num = ''
            for char in line:
                if char == 'L' or char == 'R':
                    if curr_num:
                        moves.append(Move(MoveType.FORWARD, int(curr_num)))
                        curr_num = ''
                    moves.append(Move(char, None))
                else:
                    curr_num += char
            if curr_num:
                moves.append(Move(MoveType.FORWARD, int(curr_num)))
    
    return jungle, moves
    
    

    
def print_map(jungle):
    for r in range(0, max(jungle.keys()) + 1):
        for c in range(0, max(jungle[r].keys() or [0]) + 1):
            print(jungle[r][c], end='')
        print()


def part1(use_input=False, value=None):
    jungle, path = parse_input(initialize(use_input, value))
    
    print('path:')
    for p in path:
        print(p)
        
    print('map:')
    print(jungle)
    # print_map(jungle)
    
    facing = Dir.RIGHT.value
    row = min(jungle.keys())
    col = min(jungle[row].keys())
    for move, dist in path:
        if move == MoveType.FORWARD:
            # go forward until wall
            r_delta, c_delta = DIRECTIONS[facing]
            for i in range(dist):
                row_new = row + r_delta
                col_new = col + c_delta
                
                if jungle[row_new][col_new] == TileContent.AIR:
                    print(f'Walking into air at r={row_new}, c={col_new}, facing={Dir(facing)}')
                    r_wrap = row - r_delta
                    c_wrap = col - c_delta
                    
                    while jungle[r_wrap][c_wrap] != TileContent.AIR:
                        r_wrap = r_wrap - r_delta
                        c_wrap = c_wrap - c_delta
                    
                    row_new = r_wrap + r_delta
                    col_new = c_wrap + c_delta
                    
                    print(f'  wrapped r={row_new}, c={col_new}, tile= "{jungle[row_new][col_new]}"')
                    
                if jungle[row_new][col_new] == TileContent.WALL:
                    # Hit a wall!
                    break
                
                row = row_new
                col = col_new
        elif move == MoveType.TURN_RIGHT:
            facing = (facing + 1) % 4
        elif move == MoveType.TURN_LEFT:
            facing = (facing + 3) % 4
    
    password = 1000 * row + 4 * col + facing
    print(f'row={row}, col={col}, facing={facing}, password={password}')
    return password
    


def part2(use_input=False, value=None):
    jungle, path = parse_input(initialize(use_input, value))
    
    