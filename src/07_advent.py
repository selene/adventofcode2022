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

COMMAND_PREFIX = '$'
COMMAND_CHANGE_DIR = 'cd'
COMMAND_LIST = 'ls'

DIR_ROOT = '/'
DIR_UP = '..'

LIST_DIR_PREFIX = 'dir'


DISK_SIZE = 70000000
FREE_SPACE_NEEDED = 30000000



class Directory():
    def __init__(self, name, parent, subdirs=None, files=None):
        self.name = name          # String
        self.parent = parent      # Directory or None
        
        self.subdirs = subdirs or {}
        self.files = files or []
        
        self._full_path = None
    
    def full_path(self):
        if self._full_path:
            return self._full_path
        
        path_parts = [self.name]
        curr = self.parent
        while curr:
            path_parts.append(curr.name)
            curr = curr.parent
        
        path_parts.reverse()
        self._full_path = '/'.join(path_parts)
        return self._full_path
        

class File():
    def __init__(self, name, size):
        self.name = name
        self.size = size
        

def initialize(use_input=False, value=None):
    if use_input:
        f = open('07input.txt')
        input_lines = [l.strip() for l in f.readlines()]
        f.close()
    elif value:
        input_lines = value.split('\n')
    else:
        input_lines = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k""".split('\n')
    
    return [line.split(' ') for line in input_lines]


def print_dir(directory, level=0):
    indent = '  ' * level
    
    print(indent + '- ' + directory.name + '/')
    for subdir in directory.subdirs.values():
        print_dir(subdir, level+1)
    for f in directory.files:
        print(f'{indent}  - {f.name} (size={f.size})')
    
def build_filesystem(console_lines):
    root = Directory('', None)
    curr_dir = root
    
    for line in console_lines:
        if line[0] == COMMAND_PREFIX:
            if line[1] == COMMAND_CHANGE_DIR:
                new_dir_arg = line[2]
                if new_dir_arg == DIR_ROOT:
                    curr_dir = root
                elif new_dir_arg == DIR_UP:
                    curr_dir = curr_dir.parent
                else:
                    curr_dir = curr_dir.subdirs[new_dir_arg]
            # Actually don't need to do anything for ls
        else:
            # Assume we're listing files for the current dir
            if line[0] == LIST_DIR_PREFIX:
                new_dir = Directory(line[1], curr_dir)
                curr_dir.subdirs[line[1]] = new_dir
            else:
                curr_dir.files.append(File(line[1], int(line[0])))
    
    return root


def sizes_by_dir(directory):
    sizes = {}
    
    curr_size = 0
    for subdir in directory.subdirs.values():
        sub_size, sub_size_map = sizes_by_dir(subdir)
        
        sizes.update(sub_size_map)
        curr_size += sub_size
    
    curr_size += sum([f.size for f in directory.files])
    
    sizes[directory.full_path()] = curr_size
    
    return curr_size, sizes


def part1(use_input=False, value=None):
    console_lines = initialize(use_input, value)
    
    
    # Build Filesystem from commands
    root = build_filesystem(console_lines)
    
    print_dir(root)
    
    total, sizes = sizes_by_dir(root)
    
    print(f'Total = {total}')
    for k, v in sizes.items():
        print(f'{k} - {v}')
    
    sum_small = sum([size for size in sizes.values() if size <= 100000])
    print(f'sum_small {sum_small}')
    return sum_small

    
def part2(use_input=False, value=None):
    console_lines = initialize(use_input, value)
    
    
    # Build Filesystem from commands
    root = build_filesystem(console_lines)
    
    total, sizes = sizes_by_dir(root)
    
    free_space = DISK_SIZE - total
    needed_space = FREE_SPACE_NEEDED - free_space
    
    smallest_needed = min([size for size in sizes.values() if size >= needed_space])
    print(f'smallest_needed {smallest_needed}')
    return smallest_needed
    