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


Move = namedtuple('Move', ['row_delta', 'col_delta'])
Pos = namedtuple('Pos', ['row', 'col'])

DIR_MOVES = {
    'R': Move(0, 1),
    'L': Move (0, -1),
    'U': Move (-1, 0),
    'D': Move (1, 0),
}


def initialize(use_input=False, value=None):
    if use_input:
        f = open('09input.txt')
        input_lines = [l.strip() for l in f.readlines()]
        f.close()
    elif value:
        input_lines = value.split('\n')
    else:
        input_lines = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2""".split('\n')
    
    moves = []
    for line in input_lines:
        move = line.split(' ')
        moves.extend([DIR_MOVES[move[0]]] * int(move[1]))
        
    return moves



def print_path(path):
    pass
    # TODO:
    # Find max and min row and col reached
    # Print grid


def is_touching(delta):
    return abs(delta.row) <= 1 and abs(delta.col) <= 1

def should_move_straight(delta):
    return (
        abs(delta.row) >= 2 and delta.col == 0
    ) or (
        delta.row == 0 and abs(delta.col) >= 2
    )
    

def step(head_pos, tail_pos):
    """
    Parameters
    ----------
    head_pos : Pos
    tail_pos : Pos

    Returns
    -------
    new_tail_pos : Pos

    """
    
    delta = Pos(head_pos.row - tail_pos.row, head_pos.col - tail_pos.col)
    
    if is_touching(delta):
        return tail_pos
    
    if delta.row == 0:
        # Same row
        return Pos(tail_pos.row, tail_pos.col + delta.col // abs(delta.col))
    
    if delta.col == 0:
        # Same col
        return Pos(tail_pos.row + delta.row // abs(delta.row), tail_pos.col)
    
    
    # Need to move diagonally!
    if delta.row > 0 and delta.col > 0:
        return Pos(tail_pos.row + 1, tail_pos.col + 1)
    if delta.row > 0 and delta.col < 0:
        return Pos(tail_pos.row + 1, tail_pos.col - 1)
    if delta.row < 0 and delta.col > 0:
        return Pos(tail_pos.row - 1, tail_pos.col + 1)
    if delta.row < 0 and delta.col < 0:
        return Pos(tail_pos.row - 1, tail_pos.col - 1)
    
    # Should not get here...
    return tail_pos


def part1(use_input=False):
    moves = initialize(use_input)
    
    tail_pos = Pos(0, 0)
    head_pos = Pos(0, 0)
    
    tail_visited = {tail_pos: True}
    
    for move in moves:
        new_head_pos = Pos(head_pos.row + move.row_delta, head_pos.col + move.col_delta)
        new_tail_pos = step(new_head_pos, tail_pos)
        tail_visited[new_tail_pos] = True
        
        head_pos = new_head_pos
        tail_pos = new_tail_pos
    
    return len(tail_visited)
    


def print_status(segments):
    print(f"segments, {segments}")
    
    max_r = max([seg.row for seg in segments] + [0])
    min_r = min([seg.row for seg in segments] + [0])
    height = max_r - min_r + 1

    max_c = max([seg.col for seg in segments] + [0])
    min_c = min([seg.col for seg in segments] + [0])
    width = max_c - min_c + 1
    
    rope_map = []
    for r in range(height):
        rope_map.append(['.'] * width)
    
    print(f'Initialized rope map, {rope_map}')


    rope_map[0-min_r][0-min_c] = 's'
    for idx in range(len(segments) - 1, 0, -1):
        seg = segments[idx]
        rope_map[seg.row - min_r][seg.col - min_c] = 'H' if idx == 0 else str(idx)
    
    for row in rope_map:
        print(' '.join(row))
        
            

    
def part2(use_input=False, value=None):
    moves = initialize(use_input, value)
    print('\n'.join([str(m) for m in moves]))
    
    segments = [Pos(0, 0)] * 10
    HEAD = 0
    TAIL = 9
    
    tail_visited = {segments[TAIL]: True}
    
    print("====================================")
    print_status(segments)
    
    
    for move in moves:
        #print("\n====================================")
        #print(f"Moving r{move.row_delta}, c{move.col_delta}")
        head_pos = segments[HEAD]
        
        leader_pos = Pos(head_pos.row + move.row_delta, head_pos.col + move.col_delta)
        segments[HEAD] = leader_pos
        #print(f'new leader pos {leader_pos}')
        for idx in range(1, len(segments)):
            segment = segments[idx]
            next_seg_pos = step(leader_pos, segment)
            
            #print(f'segment {idx}, was at {segment}, moved to {next_seg_pos}')
            
            segments[idx] = next_seg_pos
            leader_pos = next_seg_pos
        
        tail_visited[segments[TAIL]] = True
        

        #print_status(segments)
    
    print(tail_visited)
    
    return len(tail_visited)
        
        
        