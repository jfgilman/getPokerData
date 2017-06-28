"""Get poker data from tracker."""

import pyautogui
import time

numHands = 570

buttonSpot = [220, 106]

for i in range(0, numHands):
    time.sleep(0.05)
    pyautogui.click(buttonSpot)
    pyautogui.hotkey('down')
    time.sleep(0.05)
    pyautogui.click(571, 1022)
    time.sleep(0.05)
    pyautogui.hotkey('command', 'v')
    pyautogui.hotkey('return')
    pyautogui.hotkey('return')

numHands = 8172

buttonSpot = [220, 106]
buttonSpot2 = [273, 106]

for i in range(0, numHands):
    time.sleep(0.01)
    pyautogui.click(buttonSpot)
    time.sleep(0.01)
    pyautogui.click(571, 1022)
    time.sleep(0.01)
    pyautogui.hotkey('command', 'v')
    pyautogui.hotkey('return')
    pyautogui.typewrite('---')
    pyautogui.hotkey('return')
    time.sleep(0.01)
    pyautogui.click(buttonSpot2)
    pyautogui.hotkey('down')
    time.sleep(0.01)
    pyautogui.click(571, 1022)
    time.sleep(0.01)
    pyautogui.hotkey('command', 'v')
    pyautogui.hotkey('return')
    pyautogui.hotkey('return')
