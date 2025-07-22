# Load required files
source('R/provider-interface.R')
source('R/provider-config.R') 
source('R/provider-utils.R')
source('R/providers/mapbox-provider.R')
source('R/provider-factory.R')
source('R/token-management/token-store.R')
source('R/providers/leaflet-provider.R')

# Test basic functionality
cat('Testing LeafletProvider creation...\n')
provider <- LeafletProvider$new()
cat('✓ Provider created successfully\n')

cat('Testing provider initialization...\n')
provider$initialize_provider(list(tile_provider = 'OpenStreetMap'))
cat('✓ Provider initialized successfully\n')

cat('Testing available styles...\n')
styles <- provider$get_available_styles()
cat('✓ Found', length(styles), 'available styles\n')

cat('Testing tile provider config...\n')
config <- provider$get_tile_provider_config('OpenStreetMap')
cat('✓ Tile provider config retrieved\n')

cat('Testing view setting...\n')
provider$set_view(-74, 40.7, 10)
cat('✓ View set successfully\n')

cat('Testing layer management...\n')
layer <- list(id = 'test', type = 'ScatterplotLayer', data = data.frame(x=1, y=1))
provider$add_layer(layer)
provider$remove_layer('test')
cat('✓ Layer management works\n')

cat('Testing provider destruction...\n')
provider$destroy()
cat('✓ Provider destroyed successfully\n')

cat('\nAll Leaflet provider tests passed! ✅\n')