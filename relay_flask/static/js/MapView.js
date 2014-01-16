function Map(cssId){
	this.cssId = cssId;
	this.map = L.map('map').setView([43.673513374000002, -79.573719920000002], 14);
	this.intersections = new Object();
}

Map.prototype.setIntersections(){
	
}