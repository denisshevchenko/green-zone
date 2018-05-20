# GreenZone Manager

GreenZone Manager is a universal tool for work with GreenZone. Use this Manager to:

1. install GreenZone on your SBC,
2. re-configure installed GreenZone,
3. delete installed GreenZone from your SBC.

## Quick Start

1. Download Manager from the [stable release](https://github.com/denisshevchenko/green-zone/releases),
   for example:<br/>
   `wget -qO- https://github.com/denisshevchenko/green-zone/archive/green-zone-manager-1.0.0.zip`
2. Unpack downloaded archive:<br/>
   `unzip green-zone-manager-1.0.0.zip`
3. Run Manager:<br/>
   `./green-zone-manager`

Please make sure you have an installation permissions.

## How It Works

From a technical point of view GreenZone Manager is a Bash-script, which is using [`whiptail`](https://linux.die.net/man/1/whiptail)
program for interactive dialogue with you. Please note that `whiptail` program is pre-installed
on Debian-based Linux systems.

## Localization

In the beginning you will be asked about the language to be used during dialogue. Currently
following languages are supported:

1. English
2. Russian
