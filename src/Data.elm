module Data exposing (..)

import Dict


dataDouble : Dict.Dict String String
dataDouble =
    Dict.fromList
        [ ( "«", "»" )
        , ( "១", "!" )
        , ( "២", "ៗ" )
        , ( "៣", "ឈ" )
        , ( "៤", "\"" )
        , ( "៥", "%" )
        , ( "៦", "៍" )
        , ( "៧", "័" )
        , ( "៨", "៏" )
        , ( "៩", "(" )
        , ( "០", ")" )
        , ( "ឥ", "៌" )
        , ( "ឲ", "=" )
        , ( "ឮ", "ឭ" )
        , ( "ឆ", "ឈ" )
        , ( "ឹ", "ឺ" )
        , ( "េ", "ែ" )
        , ( "រ", "ឬ" )
        , ( "ត", "ទ" )
        , ( "យ", "ួ" )
        , ( "ុ", "ូ" )
        , ( "ិ", "ី" )
        , ( "ោ", "ៅ" )
        , ( "ផ", "ភ" )
        , ( "ៀ", "ឿ" )
        , ( "ឪ", "ឧ" )
        , ( "ា", "ា" )
        , ( "ស", "ៃ" )
        , ( "ដ", "ឌ" )
        , ( "ថ", "ធ" )
        , ( "ង", "អ" )
        , ( "ហ", "ះ" )
        , ( "្", "ញ" )
        , ( "ក", "គ" )
        , ( "ល", "ឡ" )
        , ( "ើ", "ើ" )
        , ( "់", "៉" )
        , ( "ឋ", "ឍ" )
        , ( "ខ", "ឃ" )
        , ( "ច", "ជ" )
        , ( "វ", "វ" )
        , ( "ប", "ព" )
        , ( "ន", "ណ" )
        , ( "ម", "ំ" )
        , ( "។", "៕" )
        , ( "៊", "?" )
        , ( "\x200B", " " )
        , ( "error", "Mouse" )
        ]


dataTriple =
    Dict.fromList
        [ ( "២", "@" )
        , ( "៣", "៑" )
        , ( "៤", "$" )
        , ( "៥", "€" )
        , ( "៦", "៙" )
        , ( "៧", "៚" )
        , ( "៨", "*" )
        , ( "៩", "{" )
        , ( "០", "}" )
        , ( "ឥ", "x" )
        , ( "ឲ", "៎" )
        , ( "ឮ", "\\" )
        , ( "េ", "ឯ" )
        , ( "រ", "ឫ" )
        , ( "ិ", "ឦ" )
        , ( "ោ", "ឱ" )
        , ( "ផ", "ឰ" )
        , ( "ៀ", "ឩ" )
        , ( "ឪ", "ឳ" )
        , ( "ើ", "៖" )
        , ( "់", "ៈ" )
        , ( "។", "." )
        , ( "៊", "/" )
        ]
