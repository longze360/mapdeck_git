/**
 * Baidu Maps Adapter for deck.gl Integration
 * 
 * This adapter integrates Baidu Maps API with deck.gl overlay functionality,
 * providing support for BD09 coordinate system and Chinese mapping features.
 */

// Baidu Maps adapter class
class BaiduMapAdapter {
  constructor(container, options) {
    this.container = container;
    this.options = options;
    this.map = null;
    this.deckOverlay = null;
    this.layers = new Map();
    
    // Initialize coordinate transformation utilities
    this.coordTransform = new CoordinateTransform();
    
    this.init();
  }
  
  /**
   * Initialize the Baidu map instance
   */
  init() {
    // Load Baidu Maps API if not already loaded
    if (typeof BMapGL === 'undefined') {
      this.loadBaiduAPI().then(() => {
        this.createMap();
      });
    } else {
      this.createMap();
    }
  }
  
  /**
   * Load Baidu Maps API dynamically
   */
  loadBaiduAPI() {
    return new Promise((resolve, reject) => {
      const script = document.createElement('script');
      script.src = `https://api.map.baidu.com/api?v=1.0&type=webgl&ak=${this.options.api_key}`;
      script.onload = resolve;
      script.onerror = reject;
      document.head.appendChild(script);
    });
  }
  
  /**
   * Create the Baidu map instance
   */
  createMap() {
    // Create Baidu map
    this.map = new BMapGL.Map(this.container);
    
    // Set initial view
    const center = new BMapGL.Point(
      this.options.location[0] || 116.404, 
      this.options.location[1] || 39.915
    );
    this.map.centerAndZoom(center, this.options.zoom || 11);
    
    // Set map style
    this.map.setMapStyleV2({
      styleId: this.normalizeStyle(this.options.style)
    });
    
    // Enable 3D if requested
    if (this.options.enable_3d) {
      this.map.enableTilt();
    }
    
    // Configure map controls
    this.configureControls();
    
    // Initialize deck.gl overlay
    this.initDeckOverlay();
    
    // Set up event handlers
    this.setupEventHandlers();
  }
  
  /**
   * Configure map controls based on options
   */
  configureControls() {
    // Enable/disable scroll wheel zoom
    if (this.options.enable_scroll_wheel_zoom) {
      this.map.enableScrollWheelZoom();
    } else {
      this.map.disableScrollWheelZoom();
    }
    
    // Enable/disable drag
    if (this.options.enable_drag) {
      this.map.enableDragging();
    } else {
      this.map.disableDragging();
    }
    
    // Add navigation control
    this.map.addControl(new BMapGL.NavigationControl3D());
    
    // Add scale control
    this.map.addControl(new BMapGL.ScaleControl());
  }
  
  /**
   * Initialize deck.gl overlay on Baidu map
   */
  initDeckOverlay() {
    // Create deck.gl overlay for Baidu Maps
    // Since Baidu doesn't have native deck.gl support, we create a custom overlay
    this.createCustomDeckOverlay();
  }
  
  /**
   * Create custom deck.gl overlay for Baidu Maps
   */
  createCustomDeckOverlay() {
    // Create a canvas overlay for deck.gl
    const canvas = document.createElement('canvas');
    canvas.style.position = 'absolute';
    canvas.style.top = '0';
    canvas.style.left = '0';
    canvas.style.pointerEvents = 'none';
    canvas.style.zIndex = '1000';
    
    // Add canvas to map container
    this.container.appendChild(canvas);
    
    // Initialize deck.gl with custom canvas
    this.deck = new deck.Deck({
      canvas: canvas,
      width: this.container.clientWidth,
      height: this.container.clientHeight,
      initialViewState: {
        longitude: this.options.location[0],
        latitude: this.options.location[1],
        zoom: this.options.zoom,
        pitch: this.options.pitch,
        bearing: this.options.bearing
      },
      controller: false, // Baidu map handles interaction
      layers: []
    });
    
    // Sync deck.gl view with Baidu map
    this.syncViewStates();
  }
  
  /**
   * Synchronize deck.gl view state with Baidu map
   */
  syncViewStates() {
    const updateDeckView = () => {
      const center = this.map.getCenter();
      const zoom = this.map.getZoom();
      const heading = this.map.getHeading();
      const tilt = this.map.getTilt();
      
      this.deck.setProps({
        viewState: {
          longitude: center.lng,
          latitude: center.lat,
          zoom: zoom,
          pitch: tilt,
          bearing: heading
        }
      });
    };
    
    // Listen to Baidu map events
    this.map.addEventListener('moveend', updateDeckView);
    this.map.addEventListener('zoomend', updateDeckView);
    this.map.addEventListener('rotateend', updateDeckView);
    this.map.addEventListener('tiltend', updateDeckView);
    
    // Handle resize
    window.addEventListener('resize', () => {
      this.deck.setProps({
        width: this.container.clientWidth,
        height: this.container.clientHeight
      });
    });
  }
  
  /**
   * Set up event handlers for map interactions
   */
  setupEventHandlers() {
    // Handle click events
    this.map.addEventListener('click', (e) => {
      const point = e.point;
      this.handleMapClick({
        coordinate: [point.lng, point.lat],
        pixel: [e.pixel.x, e.pixel.y]
      });
    });
    
    // Handle view state changes
    this.map.addEventListener('moveend', () => {
      this.handleViewStateChange();
    });
  }
  
  /**
   * Handle map click events
   */
  handleMapClick(event) {
    // Transform coordinates back to WGS84 for consistency
    const wgs84Coord = this.coordTransform.bd09ToWgs84(event.coordinate);
    
    // Dispatch click event to R
    if (window.Shiny) {
      Shiny.setInputValue(this.container.id + '_click', {
        coordinate: wgs84Coord,
        pixel: event.pixel,
        timestamp: Date.now()
      });
    }
  }
  
  /**
   * Handle view state changes
   */
  handleViewStateChange() {
    const center = this.map.getCenter();
    const zoom = this.map.getZoom();
    const heading = this.map.getHeading();
    const tilt = this.map.getTilt();
    
    // Transform center coordinates back to WGS84
    const wgs84Center = this.coordTransform.bd09ToWgs84([center.lng, center.lat]);
    
    // Dispatch view state to R
    if (window.Shiny) {
      Shiny.setInputValue(this.container.id + '_view_state', {
        longitude: wgs84Center[0],
        latitude: wgs84Center[1],
        zoom: zoom,
        pitch: tilt,
        bearing: heading,
        timestamp: Date.now()
      });
    }
  }
  
  /**
   * Normalize style name for Baidu Maps
   */
  normalizeStyle(style) {
    if (!style) return 'normal';
    
    // Handle style aliases
    const styleMap = {
      'normal': 'normal',
      'light': 'light',
      'dark': 'dark',
      'satellite': 'satellite',
      'hybrid': 'hybrid',
      'redalert': 'redalert',
      'googlelite': 'googlelite',
      'grassgreen': 'grassgreen',
      'midnight': 'midnight',
      'pink': 'pink',
      'darkgreen': 'darkgreen',
      'bluish': 'bluish',
      'hardedge': 'hardedge',
      'grayscale': 'grayscale'
    };
    
    return styleMap[style] || style;
  }
  
  /**
   * Add deck.gl layer
   */
  addLayer(layerConfig) {
    // Transform layer data coordinates if needed
    if (layerConfig.data) {
      layerConfig.data = this.transformLayerData(layerConfig.data);
    }
    
    // Create deck.gl layer
    const layer = this.createDeckLayer(layerConfig);
    
    // Add to deck.gl
    const currentLayers = this.deck.props.layers || [];
    this.deck.setProps({
      layers: [...currentLayers, layer]
    });
    
    // Store layer reference
    this.layers.set(layerConfig.id, layer);
  }
  
  /**
   * Remove deck.gl layer
   */
  removeLayer(layerId) {
    if (this.layers.has(layerId)) {
      const currentLayers = this.deck.props.layers || [];
      const filteredLayers = currentLayers.filter(layer => layer.id !== layerId);
      
      this.deck.setProps({
        layers: filteredLayers
      });
      
      this.layers.delete(layerId);
    }
  }
  
  /**
   * Transform layer data coordinates from WGS84 to BD09
   */
  transformLayerData(data) {
    if (!data || !Array.isArray(data)) return data;
    
    return data.map(item => {
      const transformed = { ...item };
      
      // Transform coordinate fields
      if (item.longitude !== undefined && item.latitude !== undefined) {
        const bd09Coord = this.coordTransform.wgs84ToBd09([item.longitude, item.latitude]);
        transformed.longitude = bd09Coord[0];
        transformed.latitude = bd09Coord[1];
      }
      
      if (item.position && Array.isArray(item.position) && item.position.length >= 2) {
        const bd09Coord = this.coordTransform.wgs84ToBd09([item.position[0], item.position[1]]);
        transformed.position = [bd09Coord[0], bd09Coord[1], ...(item.position.slice(2) || [])];
      }
      
      // Transform path coordinates
      if (item.path && Array.isArray(item.path)) {
        transformed.path = item.path.map(coord => {
          if (Array.isArray(coord) && coord.length >= 2) {
            const bd09Coord = this.coordTransform.wgs84ToBd09([coord[0], coord[1]]);
            return [bd09Coord[0], bd09Coord[1], ...(coord.slice(2) || [])];
          }
          return coord;
        });
      }
      
      return transformed;
    });
  }
  
  /**
   * Create deck.gl layer from configuration
   */
  createDeckLayer(config) {
    // This would create the appropriate deck.gl layer based on config.type
    // For now, return a basic configuration
    return {
      id: config.id,
      type: config.type,
      data: config.data,
      ...config.props
    };
  }
  
  /**
   * Update map style
   */
  updateStyle(style) {
    const normalizedStyle = this.normalizeStyle(style);
    this.map.setMapStyleV2({
      styleId: normalizedStyle
    });
  }
  
  /**
   * Set map view
   */
  setView(longitude, latitude, zoom, pitch, bearing) {
    // Transform coordinates from WGS84 to BD09
    const bd09Coord = this.coordTransform.wgs84ToBd09([longitude, latitude]);
    
    const center = new BMapGL.Point(bd09Coord[0], bd09Coord[1]);
    this.map.centerAndZoom(center, zoom);
    
    if (pitch !== undefined) this.map.setTilt(pitch);
    if (bearing !== undefined) this.map.setHeading(bearing);
  }
  
  /**
   * Destroy the map instance
   */
  destroy() {
    if (this.deck) {
      this.deck.finalize();
    }
    
    if (this.map) {
      // Baidu maps don't have a destroy method, just clear references
      this.map = null;
    }
    
    this.layers.clear();
  }
}

/**
 * Coordinate transformation utilities for Chinese coordinate systems
 * Extended to support BD09 transformations
 */
class CoordinateTransform {
  constructor() {
    this.x_pi = 3.14159265358979324 * 3000.0 / 180.0;
    this.pi = 3.1415926535897932384626;
    this.a = 6378245.0;
    this.ee = 0.00669342162296594323;
  }
  
  /**
   * Transform WGS84 to BD09
   */
  wgs84ToBd09(coord) {
    // WGS84 -> GCJ02 -> BD09
    const gcj02Coord = this.wgs84ToGcj02(coord);
    return this.gcj02ToBd09(gcj02Coord);
  }
  
  /**
   * Transform BD09 to WGS84
   */
  bd09ToWgs84(coord) {
    // BD09 -> GCJ02 -> WGS84
    const gcj02Coord = this.bd09ToGcj02(coord);
    return this.gcj02ToWgs84(gcj02Coord);
  }
  
  /**
   * Transform WGS84 to GCJ02
   */
  wgs84ToGcj02(coord) {
    const [lng, lat] = coord;
    
    if (this.outOfChina(lng, lat)) {
      return [lng, lat];
    }
    
    let dlat = this.transformLat(lng - 105.0, lat - 35.0);
    let dlng = this.transformLng(lng - 105.0, lat - 35.0);
    
    const radlat = lat / 180.0 * this.pi;
    let magic = Math.sin(radlat);
    magic = 1 - this.ee * magic * magic;
    const sqrtmagic = Math.sqrt(magic);
    
    dlat = (dlat * 180.0) / ((this.a * (1 - this.ee)) / (magic * sqrtmagic) * this.pi);
    dlng = (dlng * 180.0) / (this.a / sqrtmagic * Math.cos(radlat) * this.pi);
    
    return [lng + dlng, lat + dlat];
  }
  
  /**
   * Transform GCJ02 to WGS84
   */
  gcj02ToWgs84(coord) {
    const [lng, lat] = coord;
    
    if (this.outOfChina(lng, lat)) {
      return [lng, lat];
    }
    
    let dlat = this.transformLat(lng - 105.0, lat - 35.0);
    let dlng = this.transformLng(lng - 105.0, lat - 35.0);
    
    const radlat = lat / 180.0 * this.pi;
    let magic = Math.sin(radlat);
    magic = 1 - this.ee * magic * magic;
    const sqrtmagic = Math.sqrt(magic);
    
    dlat = (dlat * 180.0) / ((this.a * (1 - this.ee)) / (magic * sqrtmagic) * this.pi);
    dlng = (dlng * 180.0) / (this.a / sqrtmagic * Math.cos(radlat) * this.pi);
    
    return [lng - dlng, lat - dlat];
  }
  
  /**
   * Transform GCJ02 to BD09
   */
  gcj02ToBd09(coord) {
    const [lng, lat] = coord;
    
    const z = Math.sqrt(lng * lng + lat * lat) + 0.00002 * Math.sin(lat * this.x_pi);
    const theta = Math.atan2(lat, lng) + 0.000003 * Math.cos(lng * this.x_pi);
    
    const bd_lng = z * Math.cos(theta) + 0.0065;
    const bd_lat = z * Math.sin(theta) + 0.006;
    
    return [bd_lng, bd_lat];
  }
  
  /**
   * Transform BD09 to GCJ02
   */
  bd09ToGcj02(coord) {
    const [lng, lat] = coord;
    
    const x = lng - 0.0065;
    const y = lat - 0.006;
    const z = Math.sqrt(x * x + y * y) - 0.00002 * Math.sin(y * this.x_pi);
    const theta = Math.atan2(y, x) - 0.000003 * Math.cos(x * this.x_pi);
    
    const gcj_lng = z * Math.cos(theta);
    const gcj_lat = z * Math.sin(theta);
    
    return [gcj_lng, gcj_lat];
  }
  
  /**
   * Check if coordinates are outside China
   */
  outOfChina(lng, lat) {
    return lng < 72.004 || lng > 137.8347 || lat < 0.8293 || lat > 55.8271;
  }
  
  /**
   * Transform latitude
   */
  transformLat(lng, lat) {
    let ret = -100.0 + 2.0 * lng + 3.0 * lat + 0.2 * lat * lat + 
              0.1 * lng * lat + 0.2 * Math.sqrt(Math.abs(lng));
    ret += (20.0 * Math.sin(6.0 * lng * this.pi) + 20.0 * Math.sin(2.0 * lng * this.pi)) * 2.0 / 3.0;
    ret += (20.0 * Math.sin(lat * this.pi) + 40.0 * Math.sin(lat / 3.0 * this.pi)) * 2.0 / 3.0;
    ret += (160.0 * Math.sin(lat / 12.0 * this.pi) + 320 * Math.sin(lat * this.pi / 30.0)) * 2.0 / 3.0;
    return ret;
  }
  
  /**
   * Transform longitude
   */
  transformLng(lng, lat) {
    let ret = 300.0 + lng + 2.0 * lat + 0.1 * lng * lng + 
              0.1 * lng * lat + 0.1 * Math.sqrt(Math.abs(lng));
    ret += (20.0 * Math.sin(6.0 * lng * this.pi) + 20.0 * Math.sin(2.0 * lng * this.pi)) * 2.0 / 3.0;
    ret += (20.0 * Math.sin(lng * this.pi) + 40.0 * Math.sin(lng / 3.0 * this.pi)) * 2.0 / 3.0;
    ret += (150.0 * Math.sin(lng / 12.0 * this.pi) + 300.0 * Math.sin(lng / 30.0 * this.pi)) * 2.0 / 3.0;
    return ret;
  }
}

// Export for use in mapdeck
window.BaiduMapAdapter = BaiduMapAdapter;