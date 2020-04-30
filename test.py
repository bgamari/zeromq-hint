#!/usr/bin/env python
# -*- coding: utf-8 -*-

import zmq
import sys

ctx = zmq.Context()
s = ctx.socket(zmq.PAIR)
s.connect('tcp://127.0.0.1:5558')

for line in sys.stdin:
    s.send_json({'input': line})
    print('<', s.recv())

