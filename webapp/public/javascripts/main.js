var bratLocation = 'assets/brat';

head.js(
    // External libraries
    bratLocation + '/client/lib/jquery.min.js',
    bratLocation + '/client/lib/jquery.svg.min.js',
    bratLocation + '/client/lib/jquery.svgdom.min.js',

    // brat helper modules
    bratLocation + '/client/src/configuration.js',
    bratLocation + '/client/src/util.js',
    bratLocation + '/client/src/annotation_log.js',
    bratLocation + '/client/lib/webfont.js',

    // brat modules
    bratLocation + '/client/src/dispatcher.js',
    bratLocation + '/client/src/url_monitor.js',
    bratLocation + '/client/src/visualizer.js'
);

var webFontURLs = [
    bratLocation + '/static/fonts/Astloch-Bold.ttf',
    bratLocation + '/static/fonts/PT_Sans-Caption-Web-Regular.ttf',
    bratLocation + '/static/fonts/Liberation_Sans-Regular.ttf'
];

var collData = {
    entity_types: [ {
        "type"   : "Quantifier",
        "labels" : ["Quantifier", "Quant"],
        // Blue is a nice colour for a person?
        "bgColor": "lightblue",
        // Use a slightly darker version of the bgColor for the border
        "borderColor": "darken"
    },
    {
            "type"   : "NounPhrase",
            "labels" : ["NounPhrase", "NP"],
            // Blue is a nice colour for a person?
            "bgColor": "thistle",
            // Use a slightly darker version of the bgColor for the border
            "borderColor": "darken"
        },
     // --------------------------- Param -------------------------------------
     {
      "type": "FarmSize",
      "labels":  ["FarmSize_PARAM"],
      "bgColor": "yellow",
      "borderColor": "darken"
     },
     {
      "type": "FertilizerUse",
      "labels":  ["FertilizerUse_PARAM"],
      "bgColor": "yellow",
      "borderColor": "darken"
     },
     {
      "type": "FertilizerPrice",
      "labels":  ["FertilizerPrice_PARAM"],
      "bgColor": "yellow",
      "borderColor": "darken"
     },
     {
      "type": "HerdSize",
      "labels":  ["HerdSize_PARAM"],
      "bgColor": "yellow",
      "borderColor": "darken"
     },
     {
       "type": "HouseholdSize",
       "labels":  ["HouseholdSize_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
     },
     {
       "type": "Irrigation",
       "labels":  ["Irrigation_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
     },
     {
      "type": "Labor",
      "labels":  ["Labor_PARAM"],
      "bgColor": "yellow",
      "borderColor": "darken"
    },
     {
      "type": "PestDisease",
      "labels":  ["PestDisease_PARAM"],
      "bgColor": "yellow",
      "borderColor": "darken"
     },
     {
       "type": "Productivity",
       "labels":  ["Productivity_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
     },
     {
       "type": "Soil",
       "labels":  ["Soil_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
     },
     {
      "type": "Subsidy",
      "labels":  ["Subsidy_PARAM"],
      "bgColor": "yellow",
      "borderColor": "darken"
      },
      {
        "type": "Water",
        "labels":  ["Water_PARAM"],
        "bgColor": "yellow",
        "borderColor": "darken"
      },
      {
       "type": "Government",
       "labels":  ["Government_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
      },
      {
       "type": "Climate",
       "labels":  ["Climate_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
      },
      {
       "type": "Poverty",
       "labels":  ["Poverty_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
      },
      {
       "type": "Economy",
       "labels":  ["Economy_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
      },
      {
       "type": "Crop",
       "labels":  ["Crop_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
      },
      {
       "type": "Pollution",
       "labels":  ["Pollution_PARAM"],
       "bgColor": "yellow",
       "borderColor": "darken"
      },
      {
        "type": "Drought",
        "labels":  ["Drought_PARAM"],
        "bgColor": "yellow",
        "borderColor": "darken"
      },
      // --------------------------- AGROVOC -------------------------------------
     {
       "type": "Animal",
       "labels":  ["Animal_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Application",
       "labels":  ["Application_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Biological",
       "labels":  ["Biological_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Breeding",
       "labels":  ["Breeding_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Chemical",
       "labels":  ["Chemical_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Computer",
       "labels":  ["Computer_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Equipment",
       "labels":  ["Equipment_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Feeding",
       "labels":  ["Feeding_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Fishing",
       "labels":  ["Fishing_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Groups",
       "labels":  ["Groups_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "HumanResources",
       "labels":  ["HumanResources_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Information",
       "labels":  ["Information_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Measure",
       "labels":  ["Measure_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Natural",
       "labels":  ["Natural_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Plant",
       "labels":  ["Plant_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Population",
       "labels":  ["Population_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Research",
       "labels":  ["Research_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Site",
       "labels":  ["Site_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Stages",
       "labels":  ["Stages_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     },
     {
       "type": "Technology",
       "labels":  ["Technology_AGROVOC"],
       "bgColor": "orange",
       "borderColor": "darken"
     }

    ],

    event_types: [
      {
        "type": "Increase",
        "labels": ["INC"],
        "bgColor": "lightgreen",
        "borderColor": "darken",
        "arcs": [
            {"type": "theme", "labels": ["theme"], "borderColor": "darken", "bgColor":"violet"},
            {"type": "quantifier", "labels": ["quant"], "borderColor": "darken", "bgColor":"violet"}
        ]
      },

      {
        "type": "Decrease",
        "labels": ["DEC"],
        "bgColor": "red",
        "borderColor": "darken",
        "arcs": [
            {"type": "theme", "labels": ["theme"], "borderColor": "darken", "bgColor":"violet"},
            {"type": "quantifier", "labels": ["quant"], "borderColor": "darken", "bgColor":"violet"}
        ]
      },

      {
        "type": "Causal",
        "labels": ["CAUSAL"],
        "bgColor": "lightgreen",
        "borderColor": "darken",
        "arcs": [
          {"type": "cause", "labels": ["cause"], "borderColor": "darken", "bgColor":"pink"},
          {"type": "effect", "labels": ["effect"], "borderColor": "darken", "bgColor":"pink"}
         ]
      },
    ]
};

// docData is initially empty.
var docData = {};

head.ready(function() {

    var syntaxLiveDispatcher = Util.embed('syntax',
        $.extend({'collection': null}, collData),
        $.extend({}, docData),
        webFontURLs
    );
    var agroMentionsLiveDispatcher = Util.embed('agroMentions',
        $.extend({'collection': null}, collData),
        $.extend({}, docData),
        webFontURLs
    );

    $('form').submit(function (event) {

        // collect form data
        var formData = {
            'sent': $('input[name=sent]').val()
        }

        // process the form
        $.ajax({
            type: 'GET',
            url: 'parseSentence',
            data: formData,
            dataType: 'json',
            encode: true
        })
        .done(function (data) {
            console.log(data);
            syntaxLiveDispatcher.post('requestRenderData', [$.extend({}, data.syntax)]);
            agroMentionsLiveDispatcher.post('requestRenderData', [$.extend({}, data.agroMentions)]);
            document.getElementById("groundedAdj").innerHTML = data.groundedAdj;
        });

        // stop the form from submitting the normal way and refreshing the page
        event.preventDefault();

    });
});
