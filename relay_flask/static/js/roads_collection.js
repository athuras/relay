var RoadsCollection = Backbone.Collection.extend({
	type: RoadModel,

	fetch: function(){
		var bounds = new Object();
		bounds.minlat = -90;
		bounds.minlong = -180;
		bounds.maxlat = 90;
		bounds.maxlong = 180;

		var data = null;

		$.ajax({
			type: "POST",
			datatype: "JSON",
			contentType: "application/json",
			url: "http://localhost:5000/request_roads",
			data: JSON.stringify(bounds),
			async: false
        }).then( function(d){
        	data = d;
        });

        this.add(data);
	},
});