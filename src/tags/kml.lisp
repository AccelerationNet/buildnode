(in-package :net.acceleration.buildnode)

(eval-always
  (unless (find-package :net.acceleration.buildnode.kml)
    (defpackage :net.acceleration.buildnode.kml
	(:nicknames :kml :buildnode-kml)
      (:use :common-lisp :buildnode :iterate)
      (:shadow :when :delete :fill :open)
      (:export ))))

(in-package :net.acceleration.buildnode.kml)

(defparameter +kmlns+ "http://www.opengis.net/kml/2.2")

(defmacro kml-tags (&rest tags)
  `(progn
     ,@(iter (for tag-form in tags)
	     (for tag = (if (listp tag-form)
			    (first tag-form)
			    tag-form))
	     (for s = (if (listp tag-form)
			  (second tag-form)
			  (intern (string-upcase tag) :kml)))
	     (collect
		 `(def-tag-node :kml ,tag +kmlns+
		    ,(format nil "http://code.google.com/apis/kml/documentation/kmlreference.html#~A"
			     tag)
		    ,s))
	     (collect `(export ',s :kml)))))

(kml-tags "AbstractView" "address" "AddressDetails" "Alias" "altitude"
	  "altitudeMode" "BalloonStyle" "begin" "bgColor" "bottomFov" "Camera"
	  "Change" "color" "colorMode" "ColorStyle" "Container" "cookie" "coordinates"
	  "Create" "Data" "Delete" "description" "displayMode" "displayName" "Document"
	  "drawOrder" "east" "end" "expires" "ExtendedData" "extrude" "Feature"
	  "fill" "flyToView" "Folder" "Geometry" "gridOrigin" "GroundOverlay"
	  "heading" "href" "hotSpot" "httpQuery" "Icon" "IconStyle" "ImagePyramid"
	  "innerBoundaryIs" "ItemIcon" "key" "kml" "labelColor" "LabelStyle"
	  "latitude" "LatLonAltBox" "LatLonBox" "leftFov" "LinearRing" "LineString"
	  "LineStyle" "Link" "linkDescription" "linkName" "linkSnippet" "listItemType"
	  "ListStyle" "Location" "Lod" "longitude" "LookAt" "maxAltitude" "maxFadeExtent"
	  "maxHeight" "maxLodPixels" "maxSessionLength" "maxWidth" "message" "minAltitude"
	  "minFadeExtent" "minLodPixels" "minRefreshPeriod" "Model" "MultiGeometry"
	  "name" "near" "NetworkLink" "NetworkLinkControl" "north"
	  "Object" "open" "Orientation" "outerBoundaryIs" "outline" "Overlay"
	  "overlayXY" "Pair" "phoneNumber" "PhotoOverlay" "Placemark" "Point"
	  "Polygon" "PolyStyle" "range" "refreshInterval" "refreshMode" "refreshVisibility"
	  "Region" "ResourceMap" "rightFov" "roll" "rotation" "rotationXY"
	  "Schema" "SchemaData" "ScreenOverlay" "screenXY" "shape" "simpleData"
	  ("Scale" |Scale|) ("scale" |scale|)
	  "SimpleField" "size" "Snippet" "south" "state" "Style" "StyleMap" "StyleSelector"
	  "styleUrl" "targetHref" "tessellate" "text" "textcolor" "tileSize" "tilt"
	  "TimePrimitive" "TimeSpan" "TimeStamp" "topFov" "Update" "Url" "value"
	  "viewBoundScale" "viewFormat" "viewRefreshMode" "viewRefreshTime" "ViewVolume"
	  "visibility"
	  ;; "gx:Wait"
	  "west" "when" "width")

(defmacro with-kml-document (&body children)
  `(let ((*document* (dom:create-document
		      'rune-dom:implementation
		      nil nil
		      nil)))
     (declare (special *document*))
     (append-nodes *document* ,@children)
     *document*))

(defclass marker ()
  ((lat :accessor lat :initarg :lat :initform nil)
   (lon :accessor lon :initarg :lon :initform nil)
   (addr :accessor addr :initarg :addr :initform nil)
   (desc :accessor desc :initarg :desc :initform nil)
   (marker-name :accessor marker-name :initarg :marker-name :initform nil)
   (icon-href :accessor icon-href :initarg :icon-href :initform nil)
   ))

(defun make-marker (&key lat lon address description name icon-href)
  (make-instance 'marker
		 :addr address
		 :lat lat
		 :lon lon
		 :marker-name name
		 :desc description
		 :icon-href icon-href ))
(export 'make-marker :kml)

(defmethod build-dom ((m marker))
  (Placemark ()
    (cl:when (marker-name m)
      (name () (marker-name m)))
    (cl:when (and (lat m) (lon m))
      (Point ()
	(coordinates ()
	  (format nil "~F,~F,0" (lon m) (lat m)))))
    (cl:when (addr m)
      (kml:address () (addr m)))
    (description () (desc m))
    (style ()
      (iconstyle ()
	(icon ()
	  (href () (icon-href m)))))))

(defun make-doc-from-markers (markers &optional (title "Doc Title"))
  (with-kml-document
    (kml ()
      (Document ()
	(name () title)
	(iter  (for m in markers)
	       (collect (build-dom m)))))))
(export 'make-doc-from-markers :kml)

(defun write-test-document (&optional (f #p"test.kml") )
  (with-open-stream (fd (cl:open f :direction :output
			      :element-type '(unsigned-byte 8)
			      :if-does-not-exist :create
			      :if-exists :supersede))
    (write-document
     (make-doc-from-markers
      (list
       (make-marker :name "Acceleration.net"
		    :description "http://www.acceleration.net"
		    :icon-href "http://gainesville-green.com/images/my-home-marker.png"
		    :address
		    "2831 NW 41ST ST
Gainesville, FL 32606"))
      "Test KML Document")
     fd)))