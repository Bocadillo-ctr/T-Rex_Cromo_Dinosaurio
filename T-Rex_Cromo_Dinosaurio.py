from numpy import *
from PIL import ImageGrab, ImageOps
import pyautogui as py
import time

def salto():
    py.keyDown('space')
    time.sleep(0.2)
    py.keyUp('space')

def run():
    while True:
        if py.pixel(739,460)[0] > 84:
            pass     
        else:
            salto()
run()
