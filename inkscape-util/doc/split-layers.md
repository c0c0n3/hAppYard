Split Layers
============

Given an SVG image, this program will first remove all "lib" layers and then, for each
"regular" layer, produce a copy of the input document in which all other layers have 
been removed; if no layer is found in the input, no output is produced.

We take a layer to be any "g" element below the root which is not labeled as "master"
--i.e has an attribute whose local name is "label" and whose value equals "master",
ignoring case and leading/trailing white-space.
A "lib" layer is any "g" element below the root which is not labeled with a name
starting with "lib-", ignoring case and leading white-space.

The output files are placed in the same directory as the input and are named like the
input with an additional increasing number; e.g. if the input file name is "some.svg",
the outputs will be "some.1.svg", "some.2.svg", etc. where the number refers to the
position of the extracted layer in the input document.


Usage
-----

        split-layers some.svg

Example
-------
Given this document as input in the file "some.svg":

    <x:r xmlns:x="x-uri" xmlns="default">
        <g label="lib-1">this will be removed</g>
        <t a="a" b="b">
           some <b>text</b>
        </t>
        <g inkskape:label="master">will be ignored</g>
        <x:u a="a" b="b">
           this <g>will be ignored</g>
        </x:u>
        <g>first layer</g>
        <script>kaboom</script>
        <g x:label="lib-2">this will be removed</g>
        <g id="2"/>
    </x:r>

the following two documents would be produced

    <x:r xmlns:x="x-uri" xmlns="default">
        
        <t a="a" b="b">
           some <b>text</b>
        </t>
        <g inkskape:label="master">will be ignored</g>
        <x:u a="a" b="b">
           this <g>will be ignored</g>
        </x:u>
        <g>first layer</g>
        <script>kaboom</script>
        
        
    </x:r>

    <x:r xmlns:x="x-uri" xmlns="default">
        
        <t a="a" b="b">
           some <b>text</b>
        </t>
        <g inkskape:label="master">will be ignored</g>
        <x:u a="a" b="b">
           this <g>will be ignored</g>
        </x:u>
        
        <script>kaboom</script>
        
        <g id="2"/>
    </x:r>

respectively in the files "some.1.svg" and "some.2.svg".
