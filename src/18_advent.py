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

Cube = namedtuple('Cube', ['x', 'y', 'z'])

NEIGHBOR_DIRS = [
    Cube(0, 0, 1),
    Cube(0, 0, -1),
    Cube(0, 1, 0),
    Cube(0, -1, 0),
    Cube(1, 0, 0),
    Cube(-1, 0, 0),
]


def initialize(use_input=False, value=None):
    if use_input:
        f = open('18input.txt')
        input_lines = [l for l in f.readlines()]
        f.close()
    elif value:
        input_lines = value.split('\n')
    else:
        input_lines = """2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5""".split('\n')
    
    return [l.strip() for l in input_lines]


def parse_cubes(input_lines):
    cubes = []
    
    for line in input_lines:
        coords = [int(val) for val in line.split(',')]
        cubes.append(Cube(*coords))
    
    return cubes

    
def create_map(cubes):
    result = defaultdict(
        lambda: defaultdict(
            lambda: defaultdict(lambda: False)
            )
    )
    
    for cube in cubes:
        result[cube.x][cube.y][cube.z] = True
    
    return result


def exposed_sides(cube, cube_map):
    exposed = 0
    
    # TODO make this a Filter instead
    for n in NEIGHBOR_DIRS:
        if not cube_map[cube.x+n.x][cube.y+n.y][cube.z+n.z]:
            exposed += 1
    
    return exposed


def part1(use_input=False, value=None):
    cubes = parse_cubes(initialize(use_input, value))
    cube_map = create_map(cubes)
    
    total_exposed = sum([exposed_sides(cube, cube_map) for cube in cubes])
    print(f'Total exposed sides {total_exposed}')
    return total_exposed


def part2(use_input=False, value=None):
    cubes = parse_cubes(initialize(use_input, value))
