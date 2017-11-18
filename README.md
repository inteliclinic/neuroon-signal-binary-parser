# NeuroonRawStreamParsers

Project containing simple provisional implementation of neuroon binary streams csv parsers.
Project is implemented in Haskell - a modern purely functional programming language.

## Raison d'être

New Neuroon's firmware allows to subscribe on 2 Bluetooth Low Energy notifications characteristics.
This characteristics' notifications deliver in real-time binary frames containing data from Neuroon's sensors.
Some interface applications, suchs as Neuroon's Lucid Dreaming Research Application gather these
streams of binary frames of data into files.
This repository contains program which when compiled, converts signals from sensors
from binary frames format into common CSV.

## Installation

1. Clone repository

    ```
    git clone git@gitlab.local.inteliclinic.pl:apps/research-app-signal-stream-parser
    ```
    You may need to install your public ssh key in Gitlab.

2. Get haskell stack tool.

    ```
      curl -sSL https://get.haskellstack.org/ | sh
    ```
    or use your OS package manager.

3. Build & install using stack

    ```
      cd research-app-signal-stream-parser
      stack install
    ```

4. Parser should be installed in some directory linked to your $PATH variable such as: ~/.local/bin

## Usage

### Specify input and output files
Use it from command line like this:
```
./NeuroonRawStreamParser-exe eeg_stream.bin pat_stream.bin metadata.csv(optional) out_eeg.csv out_pat.csv
```
to parse Neuroon binary streams into two csv files.

eeg_stream.bin - file containing binary frames from Neuroon's stream0 BLE characteristic
pat_stream.bin - file containing binary frames from Neuroon's stream1 BLE characteristic
metadata.csv   - (optional) file created by lucid dream research app containing starting timestamp,
                 if not provided, the output files will use relative timestamp

out_eeg.csv    - output file for csv containing eeg data
out_pat.csv    - output file for csv containing pulsoxymeter, accelerometer and termometer data


### Process entire directory
To parse entire directory call the program without arguments. Parser will try to find groups of files describing
Neuroon binary data and parse it to csv files.

```
./NeuroonRawStreamParser-exe
```
(This will process all directory, given that input files names follow structure described below:)

Input files MUST be in the following format:
    [name_group]-[date_string_group]-[stream_identifier_grou]-[sham and extension info]
    
Each group can't contain '-' character. Stream identifier should contain lowcase string:
   - eegstream - to be read as eeg binary frames
   - patstream - to be read as pulsoxymeter, accelerometer, temperature frames.
   - metadata  - to be read as a file containing information about absolute time of recording start.

## TODO

- Better input arguments structure.
- Help messages.

## Special fix

Eventually streams should be recorded as raw original bytes.
But iOS app for Neuroon LD research saves their textual representation at the moment of writing this.
This branch's contains parser that takes it into consideration and parses textual representation of textual data.
If you are sure that this isn't correct behaviour try master branch.

## Contributing

If think you can improve the way the parser works feel free to discuss it with authors or create a merge request.

## Credits

Michał Adamczyk <m.adamczyk@inteliclinic.com>

## License

MIT License

Copyright (c) 2017 Michał Adamczyk

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
