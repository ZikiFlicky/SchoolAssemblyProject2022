@echo off
TASM src\main.asm
TLINK /v main.obj
@echo on
