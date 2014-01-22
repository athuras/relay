var bootstrap = new Object();
bootstrap.mapLayers =[
	{
		'title': 'Layer One',
		'id': 'layer-one',
		'mapStyle': 'light',
		'markerStyle': 'red',
		'styleArray': [
		  {
		    featureType: "all",
		    stylers: [
		      { saturation: -80 }
		    ]
		  },{
		    featureType: "road.arterial",
		    elementType: "geometry",
		    stylers: [
		      { hue: "#00ffee" },
		      { saturation: 50 }
		    ]
		  },{
		    featureType: "poi.business",
		    elementType: "labels",
		    stylers: [
		      { visibility: "off" }
		    ]
		  }
		]
	},{
		'title': 'Layer Two',
		'id': 'layer-two',
		'mapStyle': 'dark',
		'markerStyle': 'blue',
		'styleArray': [
			{
		    stylers: [
		      { hue: "#00ffe6" },
		      { saturation: -20 }
		    ]
		  },{
		    featureType: "road",
		    elementType: "geometry",
		    stylers: [
		      { lightness: 100 },
		      { visibility: "simplified" }
		    ]
		  },{
		    featureType: "road",
		    elementType: "labels",
		    stylers: [
		      { visibility: "off" }
		    ]
		  }
	  ]
	}
];

bootstrap.activeLayer = 'layer-one';

bootstrap.mapOptions = {
	zoom: 8,
	mapTypeId: google.maps.MapTypeId.ROADMAP,
	center: new google.maps.LatLng(42.7,-79.4)
};

bootstrap.markerStyles = {
	'red':{
		id: 'red',
		options: {
			icon: 'https://maps.google.com/mapfiles/kml/shapes/schools_maps.png'
		}
	},
	'blue': {
		id: 'blue',
		options: {
			icon: 'https://maps.google.com/mapfiles/kml/shapes/library_maps.png'
		}
	}
};