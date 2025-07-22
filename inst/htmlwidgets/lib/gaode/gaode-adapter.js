/**
 * Gaode Maps (AutoNavi) Adapter for deck.gl Integration
 * 
 * This adapter integrates Gaode Maps API with deck.gl overlay functionality,
 * providing support for GCJ02 coordinate system and Chinese mapping features.
 */

// Gaode Maps adapter class
class GaodeMapAdapter {
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
   * Initialize the Gaode map instance
   */
  init() {
    // Load Gaode Maps API if not already loaded
    if (typeof AMap === 'undefined') {
      this.loadGaodeAPI().then(() => {
        this.createMap();
      });
    } else {
      this.createMap();
    }
  }
  
  /**
   * Load Gaode Maps API dynamically
   */
  loadGaodeAPI() {
    return new Promise((resolve, reject) => {
      const script = document.createElement('script');
      script.src = `https://webapi.amap.com/maps?v=2.0&key=${this.options.api_key}&plugin=AMap.Scale,AMap.ToolBar`;
      script.onload = resolve;
      script.onerror = reject;
      document.head.appendChild(script);
    });
  }
  
  /**
   * Create the Gaode map instance
   */
  createMap() {
    // Gaode map configuration
    const mapConfig = {
      container: this.container,
      center: this.options.location || [116.397, 39.909], // Beijing default
      zoom: this.options.zoom || 10,
      pitch: this.options.pitch || 0,
      rotation: this.options.bearing || 0,
      viewMode: '3D',
      features: this.options.features || ['bg', 'road', 'building', 'point'],
      mapStyle: this.normalizeStyle(this.options.style),
      lang: this.options.lang || 'zh_cn'
    };
    
    // Create Gaode map
    this.map = new AMap.Map(this.container, mapConfig);
    
    // Add controls
    this.map.addControl(new AMap.Scale());
    this.map.addControl(new AMap.ToolBar({
      position: {
        top: '10px',
        right: '10px'
      }
    }));
    
    // Initialize deck.gl overlay
    this.initDeckOverlay();
    
    // Set up event handlers
    this.setupEventHandlers();
  }
  
  /**
   * Initialize deck.gl overlay on Gaode map
   */
  initDeckOverlay() {
    // Create deck.gl overlay for Gaode Maps
    this.deckOverlay = new deck.MapboxOverlay({
      interleaved: true,
      layers: []
    });
    
    // Custom integration for Gaode Maps
    // Since Gaode doesn't have native deck.gl support, we create a custom overlay
    this.createCustomDeckOverlay();
  }
  
  /**
   * Create custom deck.gl overlay for Gaode Maps
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
      controller: false, // Gaode map handles interaction
      layers: []
    });
    
    // Sync deck.gl view with Gaode map
    this.syncViewStates();
  }
  
  /**
   * Synchronize deck.gl view state with Gaode map
   */
  syncViewStates() {
    const updateDeckView = () => {
      const center = this.map.getCenter();
      const zoom = this.map.getZoom();
      const pitch = this.map.getPitch();
      const rotation = this.map.getRotation();
      
      this.deck.setProps({
        viewState: {
          longitude: center.lng,
          latitude: center.lat,
          zoom: zoom,
          pitch: pitch,
          bearing: rotation
        }
      });
    };
    
    // Listen to Gaode map events
    this.map.on('moveend', updateDeckView);
    this.map.on('zoomend', updateDeckView);
    this.map.on('rotateend', updateDeckView);
    this.map.on('pitchend', updateDeckView);
    
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
    this.map.on('click', (e) => {
      const lnglat = e.lnglat;
      this.handleMapClick({
        coordinate: [lnglat.lng, lnglat.lat],
        pixel: [e.pixel.x, e.pixel.y]
      });
    });
    
    // Handle view state changes
    this.map.on('moveend', () => {
      this.handleViewStateChange();
    });
  }
  
  /**
   * Handle map click events
   */
  handleMapClick(event) {
    // Transform coordinates back to WGS84 for consistency
    const wgs84Coord = this.coordTransform.gcj02ToWgs84(event.coordinate);
    
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
    const pitch = this.map.getPitch();
    const bearing = this.map.getRotation();
    
    // Transform center coordinates back to WGS84
    const wgs84Center = this.coordTransform.gcj02ToWgs84([center.lng, center.lat]);
    
    // Dispatch view state to R
    if (window.Shiny) {
      Shiny.setInputValue(this.container.id + '_view_state', {
        longitude: wgs84Center[0],
        latitude: wgs84Center[1],
        zoom: zoom,
        pitch: pitch,
        bearing: bearing,
        timestamp: Date.now()
      });
    }
  }
  
  /**
   * Normalize style name for Gaode Maps
   */
  normalizeStyle(style) {
    if (!style) return 'amap://styles/normal';
    
    // Handle style aliases
    const styleMap = {
      'normal': 'amap://styles/normal',
      'light': 'amap://styles/light',
      'dark': 'amap://styles/dark',
      'satellite': 'amap://styles/satellite',
      'hybrid': 'amap://styles/hybrid',
      'fresh': 'amap://styles/fresh',
      'grey': 'amap://styles/grey',
      'graffiti': 'amap://styles/graffiti',
      'macaron': 'amap://styles/macaron',
      'blue': 'amap://styles/blue',
      'darkblue': 'amap://styles/darkblue',
      'wine': 'amap://styles/wine'
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
   * Transform layer data coordinates from WGS84 to GCJ02
   */
  transformLayerData(data) {
    if (!data || !Array.isArray(data)) return data;
    
    return data.map(item => {
      const transformed = { ...item };
      
      // Transform coordinate fields
      if (item.longitude !== undefined && item.latitude !== undefined) {
        const gcj02Coord = this.coordTransform.wgs84ToGcj02([item.longitude, item.latitude]);
        transformed.longitude = gcj02Coord[0];
        transformed.latitude = gcj02Coord[1];
      }
      
      if (item.position && Array.isArray(item.position) && item.position.length >= 2) {
        const gcj02Coord = this.coordTransform.wgs84ToGcj02([item.position[0], item.position[1]]);
        transformed.position = [gcj02Coord[0], gcj02Coord[1], ...(item.position.slice(2) || [])];
      }
      
      // Transform path coordinates
      if (item.path && Array.isArray(item.path)) {
        transformed.path = item.path.map(coord => {
          if (Array.isArray(coord) && coord.length >= 2) {
            const gcj02Coord = this.coordTransform.wgs84ToGcj02([coord[0], coord[1]]);
            return [gcj02Coord[0], gcj02Coord[1], ...(coord.slice(2) || [])];
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
    this.map.setMapStyle(normalizedStyle);
  }
  
  /**
   * Set map view
   */
  setView(longitude, latitude, zoom, pitch, bearing) {
    // Transform coordinates from WGS84 to GCJ02
    const gcj02Coord = this.coordTransform.wgs84ToGcj02([longitude, latitude]);
    
    this.map.setCenter(gcj02Coord);
    if (zoom !== undefined) this.map.setZoom(zoom);
    if (pitch !== undefined) this.map.setPitch(pitch);
    if (bearing !== undefined) this.map.setRotation(bearing);
  }
  
  /**
   * Destroy the map instance
   */
  destroy() {
    if (this.deck) {
      this.deck.finalize();
    }
    
    if (this.map) {
      this.map.destroy();
    }
    
    this.layers.clear();
  }
}

/**
 * Coordinate transformation utilities for Chinese coordinate systems
 */
class CoordinateTransform {
  constructor() {
    this.x_pi = 3.14159265358979324 * 3000.0 / 180.0;
    this.pi = 3.1415926535897932384626;
    this.a = 6378245.0;
    this.ee = 0.00669342162296594323;
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
window.GaodeMapAdapter = GaodeMapAdapter;