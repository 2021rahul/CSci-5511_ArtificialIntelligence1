#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 31 16:00:53 2018

@author: 2021rahul
"""

def maxdepth(a):
    depth = 0
    stack = []
    a = a.split()
    for i in a:
        if i == "(":
            stack.append(i)
        elif i == ")":
            stack.pop()
            if not stack:
                depth += 1
        else:
            if not stack:
                depth += 1
    print(depth)

a = "a ( ( b c ) d ) l"
maxdepth(a)