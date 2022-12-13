# Units

Written because the author confused themselves and wanted to be sure they
now understand.

There are three main classes of coordinates in fonts:

1. Design coordinates
   * Used by the type designer and design tool
   * Not directly present in the font binary at all
   * For example, [<dimension xvalue/>](https://fonttools.readthedocs.io/en/latest/designspaceLib/xml.html#dimension) in a .designspace file
1. User, aka External, coordinates
   * The values in [fvar](https://learn.microsoft.com/en-us/typography/opentype/spec/fvar), for example Weight 400
   * What you would expose to an end user in a document editor or manipulate via CSS
1. Internal, or normalized, coordinates
   * Values in [-1, 1], used internally by the font
   * The default *must* be at 0 in these terms

Any unit can be converted to any other unit using a piecewise linear function.

## Design : Internal

Given that we have at least one design coordinate, and we know what the design coordinate for the default
outline is:

| Design coord | Internal coord | Notes |
| --- | --- | --- |
| min design value | -1 | If min < default master location |
| default master location | 0 | Mandatory |
| max design value | 1 | If max > default master location |

## User : Internal

User coordinates run through a piecewise linear function, to get internal coordinates. 
[avar](https://learn.microsoft.com/en-us/typography/opentype/spec/avar)
captures this in the font binary, derived from the User:Design mapping typically captured
 in the sources.

## User : Design

Both Glyphs and .designspace files capture mappings from user to design coordinates:

* The set of [<map>](https://fonttools.readthedocs.io/en/latest/designspaceLib/xml.html#map-element) elements for an axis capture it in .designspace sources, for example (from [Noto Sans Thai](https://github.com/notofonts/thai/blob/4af08a8f1dd3eaeb42a8dd263a326871dcdd5423/sources/NotoSansThai.designspace#L3)):

    ```xml
    <axes>
        <axis tag="wght" name="Weight" minimum="100" maximum="900" default="400">
          <map input="100" output="26"/>
          <map input="200" output="39"/>
          <map input="300" output="58"/>
          <map input="400" output="90"/>
          <map input="500" output="108"/>
          <map input="600" output="128"/>
          <map input="700" output="151"/>
          <map input="800" output="169"/>
          <map input="900" output="190"/>
        </axis>
        <axis tag="wdth" name="Width" minimum="62.500000" maximum="100" default="100">
          <map input="62.500000" output="70"/>
          <map input="75" output="79"/>
          <map input="87.500000" output="89"/>
          <map input="100" output="100"/>
        </axis>
    </axes>
  ```

* The custom key "Axis Mappings" captures it in .glyphs sources, for example (from [Lexend](https://github.com/googlefonts/lexend/blob/main/sources/Lexend.glyphs))

   ```
    {
    name = "Axis Mappings";
    value = {
        HEXP = {
            0 = 0;
            100 = 100;
        };
        wght = {
            100 = 22;
            200 = 50;
            300 = 78;
            400 = 108;
            500 = 130;
            600 = 150;
            700 = 170;
            800 = 198;
            900 = 216;
        };
    };
    }
   ```

Since design trivially maps to internal coordinates this readily yields the mapping needed for the avar table, as is the intent.

## References

See also @anthrotype's writeup in https://github.com/googlefonts/glyphsLib/issues/568#issuecomment-588982135.
