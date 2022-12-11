# -*- coding: utf-8 -*-
"""
Created on Thu Dec 02 22:16:35 2021

@author: Selene
"""

from collections import deque, namedtuple
import copy
import math
import re
import sys
import time


from parse import *

DEBUG = False


class Monkey():
    
    def __init__(self, id, items=None, operation=None, test=None, true_target=None, false_target=None):
        self.id = id
        self.items = items
        self.operation = operation
        self.test = test
        self.true_target = true_target
        self.false_target = false_target
        
        self.inspections = 0

    
    def __str__(self):
        return f'Monkey[{self.id}] items={self.items} op={self.operation} test={self.test} targets={self.true_target.id},{self.false_target.id}'

    def inspect(self):
        self.items = [self.operation(item) for item in self.items]
        self.inspections += len(self.items)
        return self.items
    
    def relax(self):    
        self.items = [math.floor(item / 3.0) for item in self.items]
        return self.items
    
    def get_throw_target(self, item):
        if self.test(item):
            return self.true_target
        else:
            return self.false_target
    
    def throw_items(self):
        for item in self.items:
            target = self.get_throw_target(item)
            print(f'     Throwing {item} to monkey {target.id}')
            target.catch(item)
            print(f'       Monkey#{target.id} items now = {target.items}')
        
        self.items.clear()
            
    
    def catch(self, item):
        self.items.append(item)
        


def initialize(use_input=False, value=None):
    if use_input:
        f = open('11input.txt')
        input_lines = [l.strip() for l in f.readlines()]
        f.close()
    elif value:
        input_lines = value.split('\n')
    else:
        input_lines = """Monkey 0:
Starting items: 79, 98
Operation: new = old * 19
Test: divisible by 23
If true: throw to monkey 2
If false: throw to monkey 3

Monkey 1:
Starting items: 54, 65, 75, 74
Operation: new = old + 6
Test: divisible by 19
If true: throw to monkey 2
If false: throw to monkey 0

Monkey 2:
Starting items: 79, 60, 97
Operation: new = old * old
Test: divisible by 13
If true: throw to monkey 1
If false: throw to monkey 3

Monkey 3:
Starting items: 74
Operation: new = old + 3
Test: divisible by 17
If true: throw to monkey 0
If false: throw to monkey 1""".split('\n')
    
    return input_lines



def create_operation(operation, value):
    val = 'old' if value == 'old' else int(value)
    
    print(f'create_operation op={operation} val={val}')
    
    if operation == '+':
        return lambda x: x + val
    if operation == '-':
        return lambda x: x - val
    if operation == '*' and val == 'old':
        return lambda x: x * x
    if operation == '*':
        return lambda x: x * val
    
    raise Exception(f'Halp unknown operation {operation}')


def create_test(operand):
    return lambda x: x % operand == 0


def parse_lines(lines):
    monkeys = []
    current_monkey = None
    
    for line in lines:
        
        monkey = parse('Monkey {id:d}:', line)
        if monkey:
            current_monkey = Monkey(monkey['id'])
            monkeys.append(current_monkey)
            print(f'Monkey id {current_monkey.id}')
            continue
        
        if 'Starting items' in line:
            items = findall('{:d}', line)
            current_monkey.items = [r[0] for r in items]
            print(f'Monkey[{current_monkey.id}].items = {current_monkey.items}')
            continue
        
        operation = parse('Operation: new = old {op} {val}', line)
        if operation:
            current_monkey.operation = create_operation(operation['op'], operation['val'])
            print(f'Monkey[{current_monkey.id}].op = {current_monkey.operation}')
            continue
        
        test = parse('Test: divisible by {num:d}', line)
        if test:
            current_monkey.test = create_test(test['num'])
            
            print(f'Monkey[{current_monkey.id}].test = {current_monkey.test}')
            continue
        
        true_target = parse('If true: throw to monkey {id:d}', line)
        if true_target:
            current_monkey.true_target = true_target['id']
            print(f'Monkey[{current_monkey.id}].true_target = {current_monkey.true_target}')
            continue
            
        false_target = parse('If false: throw to monkey {id:d}', line)
        if false_target:
            current_monkey.false_target = false_target['id']
            print(f'Monkey[{current_monkey.id}].false_target = {current_monkey.false_target}')
            continue
    
    print(f'Parsed {len(monkeys)} monkeys')
    
    for monkey in monkeys:
        monkey.true_target = monkeys[monkey.true_target]
        monkey.false_target = monkeys[monkey.false_target]
    
    return monkeys


def calculate_monkey_business(monkeys):
    inspections = sorted([m.inspections for m in monkeys], reverse=True)
    
    return inspections[0] * inspections[1]
    


def part1(use_input=False, value=None):
    monkeys = parse_lines(initialize(use_input, value))
    print('\n'.join([str(m) for m in monkeys]))
    
    rounds = 20
    for i in range(rounds):
        print(f'\n~~~ Round {i} ~~~')
        for m in monkeys:
            print(f'  ** Monkey {m.id}')
            print(f'     items = {m.items}')
            m.inspect()
            print(f'     after inspection = {m.items}')
            m.relax()
            print(f'     after relax = {m.items}')
            m.throw_items()
            
    monkey_business = calculate_monkey_business(monkeys)
    print(monkey_business)
    return monkey_business

    
def part2(use_input=False, value=None):
    monkeys = parse_lines(initialize(use_input, value))
    

