#!/usr/bin/env bash
systemPrefs="//Applications//System Preferences.app"
activityMonitor="//Applications//Utilities//Activity Monitor.app"
defaults write com.apple.dock persistent-apps -array \
  "<dict><key>tile-data</key><dict><key>file-data</key><dict><key>_CFURLString</key><string>$systemPrefs</string><key>_CFURLStringType</key><integer>0</integer></dict></dict></dict>" \
  "<dict><key>tile-data</key><dict><key>file-data</key><dict><key>_CFURLString</key><string>$systemPrefs</string><key>_CFURLStringType</key><integer>0</integer></dict></dict></dict>"
 
osascript -e 'tell application "Dock" to quit'
osascript -e 'tell application "Dock" to activate'
