@echo off
TASM /zi main.asm
TLINK /v main.obj
@echo on
