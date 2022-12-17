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
import time

DEBUG = False

class Tile:
    AIR = ' '
    ROCK = '█'
    BOUNDS = '═'


class Pos:
    def __init__(self, row, col):
        # Rows go bottom -> top, cols left -> right
        self.row = row
        self.col = col

    def __add__(self, other):
        return Pos(self.row + other.row, self.col + other.col)
    
    def __eq_(self, other):
        return self.row == other.row and self.col == other.col

    def __str__(self):
        return f'r={self.row},c={self.col}'


class Moves:
    LEFT = Pos(0, -1)
    RIGHT = Pos(0, 1)
    DOWN = Pos(-1, 0)
    
CHAR_TO_MOVE = {
    '<': Moves.LEFT,
    '>': Moves.RIGHT,
}


class RockType(IntEnum):
    MINUS = 0
    PLUS = 1
    L = 2
    I = 3
    SQUARE = 4

Rock = namedtuple('Rock', ['tiles'])
ROCK_ORDER = [RockType.MINUS, RockType.PLUS, RockType.L, RockType.I, RockType.SQUARE]
ROCK_TEMPLATES = {
    RockType.MINUS: [Pos(0,0), Pos(0,1), Pos(0,2), Pos(0,3)],
    RockType.PLUS: [Pos(0,1), Pos(1,0), Pos(1,1), Pos(1,2), Pos(2,1)],
    RockType.L: [Pos(0,0), Pos(0,1), Pos(0,2), Pos(1,2), Pos(2,2)],
    RockType.I: [Pos(0,0), Pos(1,0), Pos(2,0), Pos(3,0)],
    RockType.SQUARE: [Pos(0,0), Pos(0,1), Pos(1,0), Pos(1,1)],
}




def initialize(use_input=False, value=None):
    if use_input:
        f = open('17input.txt')
        input_lines = [l for l in f.readlines()]
        f.close()
    elif value:
        input_lines = value.split('\n')
    else:
        input_lines = """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>""".split('\n')
    
    return [l.strip() for l in input_lines]


def new_row():
    return [Tile.BOUNDS] + ([Tile.AIR] * 7) + [Tile.BOUNDS]


def create_map():
    cave = [[Tile.BOUNDS] * 9]
    for i in range(100):
        rows.append(new_row())
    
    return cave


def spawn_rock(last_rock, cave_map):
    pass


# TODO I will probably want some way to cut off the bottom of the map

    
def print_map(cave, bounds):
    for row in reversed(cave):
        print(''.join(row))
            

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
    