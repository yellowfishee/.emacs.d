English | [简体中文](./README.zh-CN.md)

## Introduction
HoloLayer is a multimedia layer plugin specifically designed for Emacs. It is developed based on PyQt, aiming to significantly enhance the visual experience of Emacs. The plugin provides a series of visual enhancement features, including modern window borders, window jump prompts, cursor animations, window screenshots, transparent floating terminals, and real-time dictionaries, etc. These features not only make the Emacs interface look more modern, but also ensure that the performance of Emacs will not be affected.

## Installation
1. Install Emacs 28 or above
2. Install Python dependencies: 
 - Linux: `pip3 install epc sexpdata six pynput inflect PyQt6 PyQt6-Qt6 PyQt6-sip` (For ArchLinux, please use pacman to install PyQt6)
 - Windows: `pip3 install epc sexpdata six pynput inflect pygetwindow PyQt6 PyQt6-Qt6 PyQt6-sip`
 - macOS: `pip3 install epc sexpdata six pynput inflect pyobjc PyQt6 PyQt6-Qt6 PyQt6-sip`
3. Download this repository using `git clone`, and replace the load-path in the configuration below
4. Add the following code to your configuration file ~/.emacs:

```elisp
(add-to-list 'load-path "<path-to-holo-layer>")
(require 'holo-layer)
(holo-layer-enable)
```

If the holo-layer hasn't started normally, please ensure that the `holo-layer-python-command` path is consistent with your system's `which python` path.

Note:
* For MacOS, please use window mode

## Demo

### Jelly Cursor Animation
<p align="center">
  <img style='height: auto; width: 80%; object-fit: contain' src="./demo/jelly-cursor.gif">
</p>

### Type Animation
<p align="center">
  <img style='height: auto; width: 80%; object-fit: contain' src="./demo/type-animation.png">
</p>

### Indent Rainbow
<p align="center">
  <img style='height: auto; width: 80%; object-fit: contain' src="./demo/indent-line.png">
</p>

### Modern Tab Line
<p align="center">
  <img style='height: auto; width: 80%; object-fit: contain' src="./demo/sort-tab-ui.png">
</p>

Need install [sort-tab](https://github.com/manateelazycat/sort-tab) first, then enable option `holo-layer-sort-tab-ui`

All icons are placed under the subdirectory `icon_cache`. You're welcome to contribute icons that you think look good. Icons can be found from websites such as https://devicon.dev or https://icons8.com/icons/set/programming-language.

To modify the color of the svg icon, just add the `style="fill: hex-color"` attribute under path.

## Options
* holo-layer-enable-cursor-animation: Whether to enable the animation of the jelly cursor, it is off by default
* holo-layer-enable-type-animation: Whether to enable the animation of the cursor type, it is off by default
* holo-layer-enable-indent-rainbow: Whehter to enable indent rainbow, it is off by default
* holo-layer-enable-place-info: Display information at the cursor in the upper right corner of the screen, such as the translation of the word at the cursor, it is off by default
* holo-layer-enable-window-border: Display window border, it is off by default
* holo-layer-hide-mode-line: Enable this option to hide the mode-line, it is off by default
* holo-layer-active-window-color: The border color of the active window, only displayed when there are more than two windows
* holo-layer-inactive-window-color: The border color of the inactive window, only displayed when there are more than two windows
* holo-layer-cursor-color: Color of the cursor for fruit-like cursor, default is nil, will follow the current background color of Emacs dynamically, or can be set to avoid dynamic change
* holo-layer-cursor-alpha: The Alpha transparency of the jelly cursor, the default is 200, completely transparent is 0, opaque is 255
* holo-layer-cursor-animation-interval: The interval time of the jelly cursor animation, the default is 10ms, don't worry about the time interval, because the animation effect is drawn with PyQt multithreading, it does not affect Emacs performance
* holo-layer-cursor-block-commands: If you don't want to display cursor animation in some cases, you can add the command string to this list, for example, after `self-insert-command` is added to the list, inserting characters will not produce animation
* holo-layer-cursor-animation-color-gradient: Enable cursor color gradient, default is t
* holo-layer-cursor-animation-color-gradient-start-value: The start value of gradient cursor color, start color is lighter 50 than cursor color
* holo-layer-type-animation-style: Cursor type animation style, can be set to `"flame"`,`"firework"`, `"firefly"`, `"matrix"`, `"supernova"`, `"lightning"`, `"balloon"`, `"hex"`,default is `"flame"`
* holo-layer-place-info-text-color: The text color of the information at the cursor, the default is the foreground color of `default`
* holo-layer-place-info-background-color: The background color of the information at the cursor, the default is the background color of `default`
* holo-layer-place-info-font-size: The font size of the information at the cursor, the default is 18
* holo-layer-window-number-color: The color of the window jump number, use the `holo-layer-jump-to-window` command to jump windows
* holo-layer-window-number-font-size: The font size of the window jump number, the default is 40
* holo-layer-sort-tab-ui: Whether to show sort-tab ui, default is nil, need install [sort-tab](https://github.com/manateelazycat/sort-tab) first
* holo-layer-sort-tab-font-size: The font size of sort-tab 

## Hyprland
Add the following configuration when using Hyprland:

```
windowrulev2 = float,title:(holo_layer.py)
windowrulev2 = nofocus,title:(holo_layer.py)
windowrulev2 = noblur,title:(holo_layer.py)
windowrulev2 = fakefullscreen,title:(holo_layer.py)
```

## Feedback Issues

For other issues, please use the command `emacs -q` and only add holo-layer configuration for a comparison test. If `emacs -q` can work normally, please check your personal configuration file.

If the problem still exists under the `emacs -q` environment, please go [here](https://github.com/manateelazycat/holo-layer/issues/new) to provide feedback, and attach the content of the `*holo-layer*` window to submit an issue to us. There are many clues in there that can help us troubleshoot the problem.

If you encounter a crash, please use the following method to collect crash information:
1. First install gdb and turn on the option `(setq holo-layer-enable-debug t)`
2. Use the command `holo-layer-stop-process` to stop the holo-layer process
3. Reopen holo-layer, and send the content of `*holo-layer*` at the next crash

## Contributors
<a href = "https://github.com/manateelazycat/holo-layer/graphs/contributors">
  <img src = "https://contrib.rocks/image?repo=manateelazycat/holo-layer"/>
</a>
