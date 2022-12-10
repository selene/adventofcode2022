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

class Operation:
    NOOP = 'noop'
    ADDX = 'addx' # Only in the input
    ADDX_START = 'addx_start'
    ADDX_FINISH = 'addx_finish'
    
    
    def __init__(self, name, value):
        self.name = name
        self.value = value



def initialize(use_input=False, value=None):
    if use_input:
        f = open('10input.txt')
        input_lines = [l.strip() for l in f.readlines()]
        f.close()
    elif value:
        input_lines = value.split('\n')
    else:
        input_lines = """noop
addx 3
addx -5""".split('\n')
    
    return [line.split(' ') for line in input_lines]



def parse_lines(lines):
    operations = []
    for line in lines:
        if line[0] == Operation.NOOP:
            operations.append(Operation(Operation.NOOP, None))
        elif line[0] == Operation.ADDX:
            operations.extend([
                Operation(Operation.ADDX_START, int(line[1])),
                Operation(Operation.ADDX_FINISH, int(line[1])),
            ])
    return operations


def should_record_strength(cycle_num):
    return cycle_num % 40 == 20 and cycle_num <= 220


def signal_strength(register, cycle_num):
    return register * cycle_num


def part1(use_input=False, value=None):
    operations = parse_lines(initialize(use_input, value))
    
    register = 1
    cycles = 1
    total_strength = 0
    for op in operations:
        # Start of cycle

        if should_record_strength(cycles):
            strength = signal_strength(cycles, register)
            total_strength += strength
            print(f"  Recorded signal {strength} at cycle {cycles} with register {register}")
        
        cycles+= 1
        
        # Not necessary, here for reference
        if op.name == Operation.NOOP or op.name == Operation.ADDX_START:
            continue
        
        # End of cycle
        if op.name == Operation.ADDX_FINISH:
            register += op.value
    
    print(total_strength)
    return total_strength


    
def part2(use_input=False, value=None):
    operations = parse_lines(initialize(use_input, value))
    
    register = 1
    cycles = 1
    beam_pos = 0
    width = 40
    for op in operations:
        cycles+= 1
        
        # Draw
        if beam_pos in [register-1, register, register+1]:
            print('█', end='')
        else:
            print('·', end='')

        # End of cycle        
        if op.name == Operation.ADDX_FINISH:
            register += op.value

        beam_pos = (beam_pos + 1) % width
        if beam_pos == 0:
            print()
        
other_sample = """addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"""