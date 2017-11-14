# NeuroonRawStreamParsers

Project containing simple provisional implementation of neuroon binary streams csv parsers.
Project is implemented in Haskell

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
      cd reserach-app-signal-stream-parser
      stack install
    ```

4. Parser should be installed in some directory linked to your $PATH variable such as: ~/.local/bin

## Usage

Use it from command line like this:
```
./NeuroonRawStreamParser-exe eeg_stream.bin pat_stream.bin metadata.csv out_eeg.csv out_pat.csv
```
eeg_stream.bin - file containing binary frames from Neuroon's stream0
pat_stream.bin - file containing binary frames from Neuroon's stream1
metadata.csv   - file created by lucid dream research app containing starting timestamp.

out_eeg.csv    - output file for csv containing eeg data
out_pat.csv    - output file for csv containing pulsoxymeter, accelerometer and termometer data

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
