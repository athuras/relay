// Setting and utility data for Relay
var bootstrap = new Object();

// Maps
bootstrap.activeLayer = 'status-layer'; // default layer

bootstrap.mapOptions = {
	zoom: 13,
	minZoom: 11,
	maxZoom: 15,
	mapTypeId: google.maps.MapTypeId.ROADMAP,
	center: new google.maps.LatLng(43.652073, -79.382293),
	disableDefaultUI: true
};

bootstrap.mapStyles = {
	'dark': [
	        {
	          "featureType": "administrative.locality",
	          "elementType": "labels.text.fill",
	          "stylers": [
	            { "color": "#a0a0a0" }
	          ]
	        },{
	          "featureType": "administrative.province",
	          "elementType": "labels.text.fill",
	          "stylers": [
	            { "color": "#b4b4b4" }
	          ]
	        },{
	          "featureType": "water",
	          "elementType": "geometry.fill",
	          "stylers": [
	            { "color": "#0b0b0b" }
	          ]
	        },{
	          "featureType": "landscape",
	          "elementType": "geometry.fill",
	          "stylers": [
	            { "color": "#313131" }
	          ]
	        },{
	          "featureType": "poi",
	          "elementType": "geometry",
	          "stylers": [
	            { "color": "#212121" }
	          ]
	        },{
	          "featureType": "poi",
	          "elementType": "labels",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "poi",
	          "elementType": "geometry.stroke",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "road",
	          "elementType": "geometry.stroke",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "road",
	          "elementType": "geometry.fill",
	          "stylers": [
	            { "color": "#171717" }
	          ]
	        },{
	          "featureType": "road",
	          "elementType": "labels.icon",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "road",
	          "elementType": "labels.text.stroke",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "road",
	          "elementType": "labels.text.fill",
	          "stylers": [
	            { "color": "#606060" }
	          ]
	        },{
	          "featureType": "administrative",
	          "elementType": "labels.icon",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "administrative",
	          "elementType": "labels.text.stroke",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "administrative.country",
	          "elementType": "labels.text.fill",
	          "stylers": [
	            { "color": "#6e6e6e" }
	          ]
	        },{
	          "featureType": "administrative.neighborhood",
	          "elementType": "labels.text.fill",
	          "stylers": [
	            { "color": "#686868" }
	          ]
	        },{
	          "featureType": "transit.line",
	          "elementType": "geometry",
	          "stylers": [
	            { "color": "#171717" }
	          ]
	        },{
	          "featureType": "transit",
	          "elementType": "labels",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "transit.station",
	          "elementType": "geometry",
	          "stylers": [
	            { "color": "#202020" }
	          ]
	        },{
	          "featureType": "water",
	          "elementType": "labels.icon",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "water",
	          "elementType": "labels.text.stroke",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "water",
	          "elementType": "labels.text.fill",
	          "stylers": [
	            { "color": "#6e6e6e" }
	          ]
	        },{
	          "elementType": "labels.icon",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "elementType": "labels.text.stroke",
	          "stylers": [
	            { "visibility": "off" }
	          ]
	        },{
	          "featureType": "landscape",
	          "elementType": "labels.text.fill",
	          "stylers": [
	            { "color": "#606060" }
	          ]
	        }
	]
};

bootstrap.markerStyles = {
	'bw_pin':{
		options: {
			// icon: '/static/assets/marker.png'
		}
	},
	'performance_glyph': {
		//templateString: 'M cx cy m -r, 0 a r,r 0 1,0 d,0 a r,r 0 1,0 -d,0',
		icon: {
	    // fillColor: 'yellow',
	    // fillOpacity: 0.8,
	    // scale: 0.5,
	    // // strokeColor: 'gold',
	    // // strokeWeight: 14
	  },
		options: {
			icon: 'https://maps.google.com/mapfiles/kml/shapes/library_maps.png'
		}
	}
};

bootstrap.heatmapStyles = {
	gradient: [
	    'rgba(0, 255, 255, 0)',
	    'rgba(0, 255, 255, 1)',
	    'rgba(0, 191, 255, 1)',
	    'rgba(0, 127, 255, 1)',
	    'rgba(0, 63, 255, 1)',
	    'rgba(0, 0, 255, 1)',
	    'rgba(0, 0, 223, 1)',
	    'rgba(0, 0, 191, 1)',
	    'rgba(0, 0, 159, 1)',
	    'rgba(0, 0, 127, 1)',
	    'rgba(63, 0, 91, 1)',
	    'rgba(127, 0, 63, 1)',
	    'rgba(191, 0, 31, 1)',
	    'rgba(255, 0, 0, 1)'
	],
	radius: 50,
	opacity: 0.8,
	dissipating: true,
	maxIntensity: 10
	// add more as you wish
};

// Charting
bootstrap.chartDataFormats = {
	labels : [],
	datasets : [
		{
			fillColor : "rgba(90,90,90,0.25)",
			strokeColor : "rgba(150,150,150,1)",
			pointColor : "rgba(150,150,150,1)",
			pointStrokeColor : "#fff",
			data : []
		},
		{
			fillColor : "rgba(88,138,190,0.1)",
			strokeColor : "rgba(88,138,190,1)",
			pointColor : "rgba(88,138,190,1)",
			pointStrokeColor : "#fff",
			data : []
		}
	]
};

bootstrap.chartOptions = {
	scaleFontFamily : "'ProximaNova', 'Helvetica Neue', 'Arial', 'sans-serif'",
	scaleFontSize : 10,
	pointDotRadius : 2,
	scaleGridLineColor : "rgba(255,255,255,0.05)"
};
