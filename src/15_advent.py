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

from parse import *


DEBUG = False


class TileContent:
    EMPTY = ' '
    UNKNOWN = '.'
    SENSOR = 'S'
    BEACON = 'B'


class Tile:
    def __init__(self, row, column, content, neighbors):
        self.row = row
        self.column = column
        self.content = content
        self.neighbors = neighbors or [None] * 4


Pos = namedtuple('Pos', ['row', 'col'])

Sensor = namedtuple('Sensor', ['pos', 'beacon', 'dist'])

# These are inclusive of both ends
# TODO figure out math/off-by-ones to make it not include end
Interval = namedtuple('Interval', ['start', 'end'])


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


def distance(pos1, pos2):
    return abs(pos1.row - pos2.row) + abs(pos1.col - pos2.col)


def has_overlap(interval1, interval2):
    if (interval1.end < interval2.start) or (interval2.end < interval1.start):
        return False
    
    return True


def merge_intervals(interval1, interval2):
    return Interval(min(interval1.start, interval2.start), max(interval1.end, interval2.end))


def initialize(use_input=False, value=None):
    if use_input:
        f = open('15input.txt')
        input_lines = [l for l in f.readlines()]
        f.close()
    elif value:
        input_lines = value.split('\n')
    else:
        input_lines = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3""".split('\n')
    
    return [line.replace('\n', '') for line in input_lines]


def parse_input(lines):
    sensor_map = defaultdict(lambda: defaultdict(lambda:  TileContent.UNKNOWN))
    sensors = []
    beacons = set()
    
    min_col = math.inf
    max_col = -math.inf
    min_row = math.inf
    max_row = -math.inf
    
    
    for line in lines:
        s_col, s_row, b_col, b_row = parse(
            'Sensor at x={:d}, y={:d}: closest beacon is at x={:d}, y={:d}',
            line)
        
        min_col = min(min_col, s_col, b_col)
        max_col = max(max_col, s_col, b_col)
        min_row = min(min_row, s_row, b_row)
        max_row = max(max_row, s_row, b_row)
        
        s_pos = Pos(s_row, s_col)
        b_pos = Pos(b_row, b_col)
        dist = distance(s_pos, b_pos)
        sensor = Sensor(s_pos, b_pos, dist)
        sensors.append(sensor)
        beacons.add(sensor.beacon)
        
        sensor_map[s_row][s_col] = TileContent.SENSOR
        sensor_map[b_row][b_col] = TileContent.BEACON
    
    print(f'Columns {min_col}-{max_col} = {max_col-min_col}, Rows {min_row}-{max_row} = {max_row-min_row}')
    
    return sensor_map, sensors, beacons, Pos(min_row, min_col), Pos(max_row, max_col)
    
    

def find_intervals(row, sensors, beacons):
    intervals = []
    
    for s in sensors:
        v_dist_from_sensor = abs(s.pos.row - row)
        if v_dist_from_sensor <= s.dist:
            # This row is in range of the sensor
            dist_in_row = s.dist - v_dist_from_sensor
            intervals.append(Interval(
                s.pos.col - dist_in_row,
                s.pos.col + dist_in_row + 1
            ))
    
    intervals = sorted(intervals, key=lambda interval: interval.start)
    
    if DEBUG:
        print('~~~ INTERVALS ~~~')
        for i in intervals:
            print(i)
    
    merged_intervals = []
    curr_ival = intervals[0]
    for ival in intervals[1:]:
        if(has_overlap(curr_ival, ival)):
            if DEBUG: print(f'Overlapping: {curr_ival}, {ival}')
            curr_ival = merge_intervals(curr_ival, ival)
            if DEBUG: print(f'--Merged: {curr_ival}')
        else:
            merged_intervals.append(curr_ival)
            curr_ival = ival
    
    merged_intervals.append(curr_ival)
    
    if DEBUG:
        print('~~~ MERGED ~~~')
        for i in merged_intervals:
            print(i)

    return merged_intervals


def count_known_empty(row, sensors, beacons):
    merged_intervals = find_intervals(row, sensors, beacons)
    sum_including_beacons = sum([
        ival.end - ival.start
        for ival in merged_intervals
    ])
    
    beacons_in_row = sum([
        1 for b in beacons if b.row == row
    ])
    
    return sum_including_beacons - beacons_in_row


def part1(use_input=False, value=None):
    sensor_map, sensors, beacons, min_pos, max_pos = parse_input(initialize(use_input, value))
    
    if use_input:
        row = 2000000
    else:
        row = 10
    
    known_empty = count_known_empty(row, sensors, beacons)
    print(f'Known empty on row {row}: {known_empty}')
    return known_empty


def part2(use_input=False, value=None):
    sensor_map, sensors, beacons, _, _ = parse_input(initialize(use_input, value))
    
    
    if use_input:
        max_row = 4000000
        max_col = 4000000
    else:
        max_row = 20
        max_col = 20
    
    beacon_row = None
    beacon_col = None
    for r in range(max_row+1):
        intervals = find_intervals(r, sensors, beacons)
        
        if len(intervals) > 1:
            beacon_row = r
            beacon_col = intervals[1].start - 1
            break
        if len(intervals) == 1:
            if intervals[0].start > 0:
                beacon_row = r
                beacon_col = 0
                break
            if intervals[0].end == max_col:
                beacon_row = r
                beacon_col = max_col
    
    print(f'beacon at r={beacon_row} c={beacon_col}')
    tuning_frequency = beacon_col * 4000000 + beacon_row
    print(f'freq = {tuning_frequency}')
    return tuning_frequency