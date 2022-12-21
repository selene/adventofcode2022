# -*- coding: utf-8 -*-
"""
Created on Thu Dec 02 22:16:35 2021

@author: Selene
"""

from collections import defaultdict, namedtuple
import copy
from enum import IntEnum
import math
import re
import sys
import time

DEBUG = False

CAVE_WIDTH = 7

class Tile:
    AIR = '.'
    ROCK = '█'
    FLOOR = '═'
    WALL = '|'
    FALLING_ROCK = '#'


class Pos:
    def __init__(self, row, col):
        # Rows go bottom -> top, cols left -> right
        self.row = row
        self.col = col

    def __add__(self, other):
        return Pos(self.row + other.row, self.col + other.col)
    
    def __iadd__(self, other):
        self.row += other.row
        self.col += other.col
        return self
    
    def __eq_(self, other):
        return self.row == other.row and self.col == other.col

    def __str__(self):
        return f'r={self.row},c={self.col}'
    
    def __repr__(self):
        return f'Pos({self.row},{self.col})'


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

#Rock = namedtuple('Rock', ['type', 'tiles'])
class Rock:
    def __init__(self, rock_type, tiles):
        self.type = rock_type
        self.tiles = tiles
    
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
    return [Tile.AIR] * 7


def create_map():
    cave = [[Tile.FLOOR] * 7]
    for i in range(10):
        cave.append(new_row())
    
    return cave


def spawn_rock(last_rock_type, cave_map):
    """
    Updates cave_map!!

    Parameters
    ----------
    last_rock_type : TYPE
        DESCRIPTION.
    cave_map : TYPE
        DESCRIPTION.

    Returns
    -------
    new_rock : TYPE
        DESCRIPTION.
    cave_map : TYPE
        DESCRIPTION.

    """
    if DEBUG: print(f'******\nspawn_rock last_type={last_rock_type}')
    
    new_type = (last_rock_type + 1) % len(ROCK_ORDER)
    new_tiles = copy.deepcopy(ROCK_TEMPLATES[new_type])
    
    for i in range(len(cave_map) - 1, -1, -1):
        row = cave_map[i]
        #print(f'{i}th row: {row}')
        if Tile.ROCK in row or Tile.FLOOR in row:
            new_pos = Pos(i + 4, 2)
            
            if DEBUG: print(f'Spawning new rock at {new_pos}')
            for tile in new_tiles:
                tile += new_pos
           
            if DEBUG: print(f'Updated tiles: {[str(t) for t in new_tiles]}')
    
            
            # Grow the cave map to fit the new spawn location
            for r in range(len(cave_map), new_pos.row + 4):
                cave_map.append(new_row())
            
            return Rock(new_type, new_tiles), cave_map


def move_rock(rock, move, cave):
    if DEBUG: print(f'move_rock rock_type={rock.type}, tiles={[str(t) for t in rock.tiles]}, move={move}')
    next_rock_tiles = [tile + move for tile in rock.tiles]
    
    if move == Moves.LEFT or move == Moves.RIGHT:
        for tile in next_rock_tiles:
            if tile.col < 0 or tile.col >= CAVE_WIDTH or cave[tile.row][tile.col] == Tile.ROCK:
                if DEBUG: print(f'move_rock bumped into something at {str(tile)}')
                # It bumped into something, don't move
                return rock
    
    return Rock(rock.type, next_rock_tiles)


def fall_rock(rock, cave):
    """
    Mutates cave!
    """
    if DEBUG: print(f'fall_rock rock_type={rock.type}, tiles={[str(t) for t in rock.tiles]}')
    next_rock = move_rock(rock, Moves.DOWN, cave)
    
    if DEBUG: print(f'fall_rock next_rock: type={next_rock.type}, tiles={[str(t) for t in next_rock.tiles]}, ')
    
    collides = False
    for tile in next_rock.tiles:
        if cave[tile.row][tile.col] == Tile.ROCK or cave[tile.row][tile.col] == Tile.FLOOR:
            collides = True
            break
    
    if collides:
        # Lock it in place from the previous value
        for tile in rock.tiles:
            cave[tile.row][tile.col] = Tile.ROCK
        return True, rock, cave
    
    return False, next_rock, cave



# TODO I will probably want some way to cut off the bottom of the map

    
def print_map(cave, rock=None):
    if rock:
        for r in range(len(cave)-1, -1, -1):
            for c in range(CAVE_WIDTH):
                is_falling = False
                for tile in rock.tiles:
                    if tile.row == r and tile.col == c:
                        is_falling = True
                        break
                if is_falling:
                    print(Tile.FALLING_ROCK, end='')
                else:
                    print(cave[r][c], end='')
            print()
                    
    else:
        for row in reversed(cave):
            print(''.join(row))
            


def part1(use_input=False, value=None):
    jet_dirs = initialize(use_input, value)[0]
    cave = create_map()
    
    curr_rock, cave = spawn_rock(-1, cave)
    curr_jet_idx = 0
    rocks_stopped = 0
    
    jet_fires = True
    while rocks_stopped < 2022:
        if jet_fires:
            jet_dir = CHAR_TO_MOVE[jet_dirs[curr_jet_idx]]
            curr_rock = move_rock(curr_rock, jet_dir, cave)
            curr_jet_idx = (curr_jet_idx + 1) % len(jet_dirs)
            jet_fires = False
        else:
            stopped, curr_rock, cave = fall_rock(curr_rock, cave)
            if stopped:
                rocks_stopped += 1
                #print(f'==== After rock {rocks_stopped} ====')
                if DEBUG: print_map(cave)
                curr_rock, cave = spawn_rock(curr_rock.type, cave)
            jet_fires = True
        
        if DEBUG: print_map(cave, curr_rock)

    print('~~~~~~~~ FINAL SITUATION ~~~~~~')
    print_map(cave[:100])
    
    for i in range(len(cave)-1, 0, -1):
        row = cave[i]
        if Tile.ROCK in row or Tile.FLOOR in row:
            print(f'Highest rock is {i}')
            return i

def part2(use_input=False, value=None):
    jet_dirs = initialize(use_input, value)
    cave = create_map()
    
    
    ##### PLAN
    # Pick a desired array height
    # Whenever it reaches 2x that, cut off the bottom half and add to an offset
    # Update all the functions to take an offset of some kind
    # OR ELSE just track it at the top level, and only update the falling rock's tiles for the new offset
    
    
    