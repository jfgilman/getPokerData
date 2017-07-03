"""Get poker data from tracker."""

import pyautogui
import time

numHands = 1102

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

numHands = 15

buttonSpot = [220, 106]
buttonSpot2 = [273, 106]

for i in range(0, numHands):
    pyautogui.click(buttonSpot)
    time.sleep(0.03)
    pyautogui.click(571, 1022)
    time.sleep(0.03)
    pyautogui.hotkey('command', 'v')
    pyautogui.hotkey('return')
    pyautogui.typewrite('---')
    pyautogui.hotkey('return')
    pyautogui.click(buttonSpot2)
    pyautogui.hotkey('down')
    time.sleep(0.03)
    pyautogui.click(571, 1022)
    time.sleep(0.03)
    pyautogui.hotkey('command', 'v')
    pyautogui.hotkey('return')
    pyautogui.hotkey('return')
