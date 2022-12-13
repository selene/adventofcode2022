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
        f = open('13input.txt')
        input_lines = [l for l in f.readlines()]
        f.close()
    elif value:
        input_lines = value.split('\n')
    else:
        input_lines = """[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]""".split('\n')
    
    return lines_to_pairs([l.strip() for l in input_lines])


def lines_to_pairs(lines):
    pairs = []
    curr_pair = []
    
    for line in lines:
        if line:
            curr_pair.append(eval(line))
        else:
            pairs.append(curr_pair)
            curr_pair = []
        
    pairs.append(curr_pair)
    return pairs


def is_list(val):
    return isinstance(val, list)


def in_right_order(val1, val2, verbose=False, depth=0):
    indent = '  ' * depth
    if verbose:
        print(f'\n{indent}Comparing pair:')
        print(f'{indent}* {val1}')
        print(f'{indent}* {val2}')
    val1_is_list = is_list(val1)
    val2_is_list = is_list(val2)
    
    if val1_is_list and val2_is_list:
        if verbose: print(indent + 'Both lists')
        val1_len = len(val1)
        val2_len = len(val2)
        
        for i in range(min(val1_len, val2_len)):
            if verbose: print(f'{indent}  Testing index {i}')
            curr_in_order = in_right_order(val1[i], val2[i], verbose, depth+1)
            if verbose: print(f'{indent}  --Result: {curr_in_order}')
            if curr_in_order is not None:
                return curr_in_order
        if verbose: print(f'{indent}  Lengths: {val1_len}, {val2_len}')
        if val1_len < val2_len:
            return True
        elif val1_len > val2_len:
            return False
        return None
    elif (not val1_is_list) and (not val2_is_list):
        if verbose: print(f'{indent}  both integers')
        if val1 < val2:
            return True
        if val1 > val2:
            return False
        return None    # Need to check next element
    else:
        if val1_is_list:
            if verbose: print(f'{indent}  Wrapping in list val2={val2}')
            return in_right_order(val1, [val2], verbose, depth+1)
        else:
            if verbose: print(f'{indent}  Wrapping in list val1={val1}')
            return in_right_order([val1], val2, verbose, depth+1)
    
    return None



def part1(use_input=False, value=None):
    pairs = initialize(use_input, value)
    
    right_sum = 0
    for i, pair in enumerate(pairs):
        is_in_order = in_right_order(pair[0], pair[1], True)
        if is_in_order:
            print(f'Pair {i+1} is in the right order')
            right_sum += i+1
    
    print(f'Right sum {right_sum}')
    return right_sum
    


def part2(use_input=False, value=None):
    pass





