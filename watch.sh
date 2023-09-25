#!/bin/bash

find . -name "*.clj" | entr -c clj -M:test
