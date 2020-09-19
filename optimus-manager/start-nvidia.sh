#!/bin/bash

mv ~/.i3/config ~/.i3/config-original
mv ~/.i3/config-nvidia ~/.i3/config
optimus-manager --switch nvidia