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


def initialize(use_input=False, value=None):
    if use_input:
        f = open('08input.txt')
        input_lines = [l.strip() for l in f.readlines()]
        f.close()
    elif value:
        input_lines = value.split('\n')
    else:
        input_lines = """30373
25512
65332
33549
35390""".split('\n')
    
    tree_map = []
    base_width = len(input_lines[0])
    base_height = len(input_lines)

 
    for line in input_lines:
        tree_map.append([int(c) for c in line])
        
    return tree_map, base_width, base_height



def print_map(tree_map):
    for row in tree_map:
        print(' '.join(['..' if c == 0 else '🌲' for c in row]))

def part1(use_input=False):
    tree_map, width, height = initialize(use_input)
    
    visible_map = []
    for r in range(height):
        visible_map.append([0] * width)
        
    
    # From left
    for ridx, row in enumerate(tree_map):
        max_height = -1
        for cidx, tree_height in enumerate(row):
            if tree_height > max_height:
                visible_map[ridx][cidx] = 1
                max_height = tree_height
    
    # From right
    for ridx, row in enumerate(tree_map):
        max_height = -1
        for cidx in reversed(range(width)):
            tree_height = tree_map[ridx][cidx]
            # print(f"From right, r={ridx} c={cidx} max_height={max_height} tree_height={tree_height}")
            if tree_height > max_height:
                visible_map[ridx][cidx] = 1
                max_height = tree_height

    # From top
    for cidx in range(width):
        max_height = -1
        for ridx in range(height):
            tree_height = tree_map[ridx][cidx]
            if tree_height > max_height:
                visible_map[ridx][cidx] = 1
                max_height = tree_height

    # From bottom
    for cidx in range(width):
        max_height = -1
        for ridx in reversed(range(height)):
            tree_height = tree_map[ridx][cidx]
            if tree_height > max_height:
                visible_map[ridx][cidx] = 1
                max_height = tree_height

    visible_count = sum([sum(row) for row in visible_map])
    
    print_map(visible_map)
    print(visible_count)
    return visible_count
    

Dir = namedtuple('Dir', ('rd', 'cd'))  # row delta and col delta

def part2(use_input=False):
    tree_map, width, height = initialize(use_input)
    
    highest_score = -1
    
    for ridx in range(1, (height-1)):
        for cidx in range(1, (width-1)):
            # print(f"\nTree r={ridx} c={cidx}")
            dirs = [
                Dir(0, 1),
                Dir(0, -1),
                Dir(1, 0),
                Dir(-1, 0),
            ]
            
            tree_height = tree_map[ridx][cidx]
            # print(f"  height {tree_height}")
            score = 1
            for d in dirs:
                #print(f"  dir{d}")
                dist = 0
                r = ridx + d.rd
                c = cidx + d.cd
                
                while r >= 0 and r < height and c >= 0 and c < width:
                    # print(f"  -> checking r={r} c={c}, height={tree_map[r][c]}")
                    dist += 1
                    if tree_map[r][c] >= tree_height:
                        break
                    
                    r = r + d.rd
                    c = c + d.cd
                # print(f"  final dist {dist}")
                score = score * dist
            
            # print(f"  final score {score}")
            
            if score > highest_score:
                highest_score = score
    
    print(highest_score)
    return highest_score
                    
                    
            
    
    