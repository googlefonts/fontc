Script to fetch and compile an "interesting" set of Google Fonts.

May in time validate we get something that looks sane ... but not yet.

See https://github.com/googlefonts/fontmake-rs/issues/50 for context.

```shell
# Sample usage
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
python smoke_test.py
```