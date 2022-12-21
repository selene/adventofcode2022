# -*- coding: utf-8 -*-
"""
Created on Thu Dec 02 22:16:35 2021

@author: Selene
"""

from collections import defaultdict, deque, namedtuple
import copy
from enum import Enum
import math
import re
import sys
import time

DEBUG = False


class Op(Enum):
    Equal = '='
    Plus = '+'
    Minus = '-'
    Times = '*'
    Divide = '/'


Monkey = namedtuple('Monkey', ['name', 'op', 'val1', 'val2'])


def initialize(use_input=False, value=None):
    if use_input:
        f = open('21input.txt')
        input_lines = [l for l in f.readlines()]
        f.close()
    elif value:
        input_lines = value.split('\n')
    else:
        input_lines = """root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32""".split('\n')
    
    return [l.strip() for l in input_lines]


def parse_input(lines):
    monkeys = {}
    for line in lines:
        print(f'Parsing "{line}"')
        parts = line.split(': ')
        name = parts[0]
        op = Op.Equal
        val1 = None
        val2 = None
        try:
            val1 = int(parts[1])
        except ValueError:
            if Op.Plus.value in parts[1]:
                op = Op.Plus
                val1, val2 = parts[1].split(' + ')
            elif Op.Minus.value in parts[1]:
                op = Op.Minus
                val1, val2 = parts[1].split(' - ')
            elif Op.Times.value in parts[1]:
                op = Op.Times
                val1, val2 = parts[1].split(' * ')
            elif Op.Divide.value in parts[1]:
                op = Op.Divide
                val1, val2 = parts[1].split(' / ')
        else:
            op = Op.Equal
        
        monkeys[name] = Monkey(name, op, val1, val2)
    
    return monkeys



def evaluate_monkey(name, monkeys):
    """
    WARNING mutates monkeys!

    Parameters
    ----------
    name : TYPE
        DESCRIPTION.
    monkeys : TYPE
        DESCRIPTION.

    Returns
    -------
    TYPE
        DESCRIPTION.
    TYPE
        DESCRIPTION.

    """
    monkey = monkeys[name]
    
    if monkey.op == Op.Equal:
        return monkey.val1, monkeys
    
    left, updated_monkeys = evaluate_monkey(monkey.val1, monkeys)
    updated_monkeys[monkey.val1] = Monkey(monkey.val1, Op.Equal, left, None)
    
    right, updated_monkeys = evaluate_monkey(monkey.val2, monkeys)
    updated_monkeys[monkey.val2] = Monkey(monkey.val2, Op.Equal, right, None)
    
    if monkey.op == Op.Plus:
        return left + right, updated_monkeys
    elif monkey.op == Op.Minus:
        return left - right, updated_monkeys
    elif monkey.op == Op.Times:
        return left * right, updated_monkeys
    elif monkey.op == Op.Divide:
        return left // right, updated_monkeys

def part1(use_input=False, value=None):
    monkeys = parse_input(initialize(use_input, value))
    print(monkeys)
    
    root_val, evaluated_monkeys = evaluate_monkey('root', monkeys)
    
    print(f'root={root_val}')
    return root_val

    
            
    


def part2(use_input=False, value=None):
    monkeys = parse_input(initialize(use_input, value))
