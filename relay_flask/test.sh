curl -X \
	POST -H "Content-Type: application/json" \
	-d '{"minlat": 43.61,"maxlat": 43.62, "minlong": -79.39, "maxlong": -79.37}' \
	 http://localhost:$1/request_intersections

