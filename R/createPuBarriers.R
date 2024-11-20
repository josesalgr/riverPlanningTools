#' @title Create River Network Planning Units with Barriers
#'
#' @description This function modifies a river network's spatial structure (`reaches_sf`) by incorporating the 
#' effects of barriers (`barriers_sf`). The function projects barriers onto the nearest river segments, adjusts 
#' the connectivity relationships (e.g., `NEXT_DOWN`), and returns an updated river network.
#'
#' @param reaches_sf An `sf` object representing the river network. It must contain the columns `HYRIV_ID` (unique 
#' identifier for each segment) and `NEXT_DOWN` (identifier for the downstream segment).
#' @param barriers_sf An `sf` object representing the barriers. Points in this dataset will be projected 
#' onto the river network.
#' @param threshold_distance Numeric. The maximum distance allowed for a barrier to be considered 
#' "close enough" to a river segment.
#' @param near_distance_barriers Numeric. The minimum distance between projected barrier points. 
#' Barriers closer than this distance will be aggregated.
#'
#' @return A list with two components:
#' \describe{
#'   \item{`divided_reaches`}{An updated `sf` object representing the river network with split segments.}
#'   \item{`projected_points`}{An `sf` object containing the projected locations of the barriers.}
#'   \item{`final_reaches`}{An `sf` object containing the final reaches.}
#' }
#'
#' @examples
#' \donttest{
#'  # Example usage
#'  result <- createPuBarriers(reaches_sf, barriers_sf, threshold_distance = 100, near_distance_barriers = 10)
#'  updated_reaches <- result$updated_reaches
#'  projected_points <- result$projected_points
#' }
#'
#' @rdname createPuBarriers
#' @export
#' @import sf lwgeom data.table dplyr igraph

createPuBarriers <- function(reaches_sf, 
                             barriers_sf, 
                             threshold_distance = 100, 
                             near_distance_barriers = 10,
                             id = "GridID", nxt = "NextDownID") {
  
  # Check if required columns exist in reaches_sf
  required_columns <- c(id, nxt)
  if (!all(required_columns %in% names(reaches_sf))) {
    stop(paste0("Error: '", id, "' or '", nxt, "' column is missing in reaches_sf"))
  }
  # Select only required columns
  reaches_sf <- reaches_sf[, required_columns, drop = FALSE]
  
  # Ensure barriers_sf has the same CRS as reaches_sf
  
  if (st_crs(barriers_sf) != st_crs(reaches_sf)) {
    cat("Transforming CRS's... \n")
    barriers_sf <- st_transform(barriers_sf, st_crs(reaches_sf))
  }

  # Buffer radius for splitting river segments
  buffer_radius <- 2
  
  # Initialize an empty sf object to store projected barrier points
  projected_points <- st_sf(n = NULL, geometry = st_sfc(), crs = st_crs(barriers_sf))
  
  for (i in seq_len(nrow(barriers_sf))) {
    # Find the nearest river segment for the current barrier
    nearest_line_index <- st_nearest_feature(barriers_sf[i, ], reaches_sf)
    nearest_line <- reaches_sf[nearest_line_index, ]
    id_nearest_line <- nearest_line[[id]]
    
    # Calculate the distance from the barrier to the nearest river segment
    distancia <- st_distance(barriers_sf[i, ], nearest_line)
    cat(paste0("(", i, "/", nrow(barriers_sf), ") distance: ", distancia, "\n"))
    
    # Skip if the distance exceeds the threshold
    if (as.numeric(distancia) > threshold_distance) {
      next
    }
    
    # Project the barrier onto the nearest river segment
    projection <- st_nearest_points(barriers_sf[i, ], nearest_line)
    projection_points <- st_cast(projection, "POINT")
    
    # Check for nearby projected points to avoid redundancy
    nearest_index <- st_nearest_feature(projection_points[2], projected_points)
    if (!is.na(nearest_index)) {
      distances_to_points <- as.numeric(st_distance(projection_points[2], projected_points[nearest_index, ]))
      if (distances_to_points < near_distance_barriers) {
        projected_points[nearest_index, ]$n <- projected_points[nearest_index, ]$n + 1
        next
      }
    }
    
    # Create a buffer around the projected point to split the river segment
    buffer <- st_buffer(projection_points[2], dist = buffer_radius)
    split_line <- lwgeom::st_split(nearest_line, buffer)
    
    # Extract the split LINESTRING components
    lineas_divididas <- st_collection_extract(split_line$geometry, "LINESTRING")
    
    # Process split lines to update river network connectivity
    lineas_divididas <- lineas_divididas[c(1, length(lineas_divididas))]
    
    id_nearest_line_nexdown <- nearest_line[[nxt]]
    
    ids_new <- c(max(reaches_sf[[id]]) + 1, max(reaches_sf[[id]]) + 2)
    nearest_line_as_nextDown <- which(reaches_sf[[nxt]] == id_nearest_line)
    
    if (id_nearest_line_nexdown != 0) {
      dist_line1 <- st_distance(lineas_divididas[1], reaches_sf[reaches_sf[[id]] == id_nearest_line_nexdown, ])
      dist_line2 <- st_distance(lineas_divididas[2], reaches_sf[reaches_sf[[id]] == id_nearest_line_nexdown, ])
      
      if (dist_line1[[1]] < dist_line2[[1]]) {
        nextDown_new <- c(ids_new[2], id_nearest_line_nexdown)
        
        if (length(nearest_line_as_nextDown) != 0) {
          reaches_sf[nearest_line_as_nextDown, ][[nxt]] <- ids_new[1]
        }
      } else if (dist_line2[[1]] < dist_line1[[1]]) {
        nextDown_new <- c(ids_new[2], id_nearest_line_nexdown)
        
        if (length(nearest_line_as_nextDown) != 0) {
          reaches_sf[nearest_line_as_nextDown, ][[nxt]] <- ids_new[1]
        }
      }
    } else {
      row_nf1 <- st_nearest_feature(lineas_divididas[1], y = reaches_sf[-c(nearest_line_index), ])
      row_nf2 <- st_nearest_feature(lineas_divididas[2], y = reaches_sf[-c(nearest_line_index), ])
      
      id_nf1 <- reaches_sf[-c(nearest_line_index), ][[id]][row_nf1]
      id_nf2 <- reaches_sf[-c(nearest_line_index), ][[id]][row_nf2]
      
      if (reaches_sf[reaches_sf[[id]] == id_nf1, ][[nxt]] == id_nearest_line) {
        nextDown_new <- c(ids_new[2], id_nearest_line_nexdown)
        
        if (length(nearest_line_as_nextDown) != 0) {
          reaches_sf[nearest_line_as_nextDown, ][[nxt]] <- ids_new[1]
        }
      } else if (reaches_sf[reaches_sf[[id]] == id_nf2, ][[nxt]] == id_nearest_line || id_nearest_line_nexdown == 0) {
        nextDown_new <- c(ids_new[2], id_nearest_line_nexdown)
        if (length(nearest_line_as_nextDown) != 0) {
          reaches_sf[nearest_line_as_nextDown, ][[nxt]] <- ids_new[1]
        }
      } else {
        cat("Error: It's impossible to assign NextDown ID")
      }
    }
    
    # Add new segments to the river network
    nuevas_lineas <- st_sf(
      HYRIV_ID = ids_new,
      NEXT_DOWN = nextDown_new,
      geometry = lineas_divididas
    )
    
    # Renombrar columnas dinámicamente
    colnames(nuevas_lineas)[1:2] <- c(id, nxt)
    
    reaches_sf <- reaches_sf[-nearest_line_index, ]
    reaches_sf <- dplyr::bind_rows(reaches_sf, nuevas_lineas)
    
    # Save the projected point
    projected_points <- dplyr::bind_rows(projected_points, 
                                  st_sf(id = i, n = 1, geometry = st_sfc(projection_points[2]), crs = st_crs(barriers_sf)))
  }
  
  
  
  # Cluster of reaches
  # Intersect the lines to identify connected components
  intersections <- st_intersects(reaches_sf)
  graph <- igraph::graph_from_adj_list(intersections, mode = "all")
  
  # Assign a group to each connected set of lines
  reaches_sf$group <- components(graph)$membership
  
  # Merge lines within the same group and aggregate relevant attributes
  lines_union <- reaches_sf %>%
    group_by(group) %>%
    summarise(
      geometry = st_union(geometry),  # Combine geometries
      HYRIV_ID_list = list(!!sym(id)),   # Store the {{ id }} of the group as a list
      NEXT_DOWN_list = list(!!sym(nxt))  # Store the {{ nxt }} of the group as a list
    ) %>%
    rowwise() %>%
    ungroup()
  
  # Initialize new columns for the unique {{ id }} and {{ nxt }} in the merged groups
  lines_union[[id]] <- NA
  lines_union[[nxt]] <- NA
  
  # Loop through the groups to assign unique {{ id }} and {{ nxt }}
  for (i in 1:nrow(lines_union)) {
    vector_ids <- unlist(lines_union[i, ]$HYRIV_ID_list)
    
    if (length(vector_ids) != 1) {
      vector_nextdown_ids <- unlist(lines_union[i, ]$NEXT_DOWN_list)
      vector_in <- (vector_nextdown_ids %in% vector_ids)
      
      if (length(which(!vector_in)) == 1) {
        new_ID <- vector_ids[which(!vector_in)]
        new_nextdown <- vector_nextdown_ids[which(!vector_in)]
        
        lines_union[i, ][[id]] <- new_ID
        lines_union[i, ][[nxt]] <- new_nextdown
        
      } else {
        if (all(vector_nextdown_ids[!vector_in] == 0)) {
          lines_union[i, ][[id]] <- vector_ids[which(!vector_in)][1]
          lines_union[i, ][[nxt]] <- vector_nextdown_ids[which(!vector_in)][1]
        } else {
          stop(paste0("Error: id ", i, ", with length: ", length(which(!vector_in))))
        }
      }
    } else {
      lines_union[i, ][[id]] <- unlist(lines_union[i, ]$HYRIV_ID_list)
      lines_union[i, ][[nxt]] <- unlist(lines_union[i, ]$NEXT_DOWN_list)
    }
  }
  
  # Update {{ nxt }} references to point to merged groups
  for (i in 1:nrow(lines_union)) {
    if (lines_union[[nxt]][i] != 0) {
      id_nextdown <- lines_union[i, ][[nxt]]
      
      indices <- which(sapply(lines_union$HYRIV_ID_list, function(x) id_nextdown %in% x))
      
      if (length(indices) != 0) {
        lines_union[i, ][[nxt]] <- lines_union[indices, ][[id]]
      }
    }
  }
  
  # Select the final relevant columns, removing intermediate ones
  lines_union <- lines_union %>%
    select(!!sym(id), !!sym(nxt), geometry, -all_of(c("HYRIV_ID_list", "group", "NEXT_DOWN_list")))
  
  return(list(divided_reaches = reaches_sf, 
              projected_points = projected_points,
              final_reaches = lines_union))
}



#How to use---------------------------------------------------------
#From shapefile data
# Name_reaches_shp = "C:/Users/joses/Dropbox/Projects/Opened/AnguilaVirgilio/HydroRIVERS_v10_eu_shp/HydroRIVERS_v10_eu_shp"
# file_reaches = sf::read_sf(Name_data_shp)
# # 
# library(rnaturalearth)
#  
# espana_limite <- ne_states(country = "Spain", returnclass = "sf")
# espana_limite_completo <- st_union(espana_limite)
#  
# # # Asegurarse de que ambos shapefiles tienen el mismo sistema de coordenadas
# espana_limite_completo <- st_transform(espana_limite_completo, st_crs(file_reaches))
# # 
# # # Crear un buffer alrededor del punto
# buffer_espana_limite_completo <- st_buffer(espana_limite_completo, dist = 500000)
# # 
# # # st_write(buffer_espana_limite_completo, "outputs/limite_buffer500000.shp")
# # 
# # # 2. Recortar las líneas dentro del límite administrativo de España
# lineas_dentro_espana <- st_intersection(file_reaches, buffer_espana_limite_completo)
# 
# Name_dams_shp = "C:/Users/joses/Dropbox/Projects/Opened/AnguilaVirgilio/shapes/barreras_snaped.shp"
# file_dams = sf::read_sf(Name_dams_shp)
# 
# 
# #Function
# file_dams <- file_dams[1:10, ]
#  
# out <- createPuBarriers(reaches_sf = lineas_dentro_espana,
#                   barriers_sf = file_dams,
#                   id = "HYRIV_ID",
#                   nxt = "NEXT_DOWN")