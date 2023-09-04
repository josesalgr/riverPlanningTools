#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame timesTwo(DataFrame x, int connection_limit, int distance_limit, String df_id,
              String df_nxt, String df_len) {
  
  //creating empty data frame to distances
  std::vector<int> id;
  std::vector<int> down;
  std::vector<double> distance;
  
  int row_distances_data = 0;
  int length_df = x.nrows();
  std::vector<int> GridID = x[df_id];
  std::vector<int> NextDownID = x[df_nxt];
  std::vector<double> Length = x[df_len];

  for(int i=0; i<length_df; i++)
  {
    //std::cout << i << std::endl;
    
    //internal parameters
    int row_df = i;
    int count_distance = 0;
    int n_connections = 0;
    bool in_memory = false;
  
    while(NextDownID[row_df] != -1)
    {
      //stop by number of connections
      if(n_connections == connection_limit || count_distance + Length[row_df] >= distance_limit){
        break;
      }
      
      //adding next down cell to distances data frame
      id.push_back(GridID[i]);
      down.push_back(NextDownID[row_df]);
      distance.push_back(count_distance + Length[row_df]);
      count_distance = count_distance + Length[row_df];

      //std::cout << "i: " << GridID[i] << " down: "<< NextDownID[row_df] << std::endl;
      
      
      //verify if it still exists int the data.frame 
      // auto output_id = find(id.begin(), id.end(), NextDownID[row_df]) - id.begin();
      // int id_size = id.size();
      // if(output_id != id_size){
      //   in_memory = true;
      //   while(id[output_id]==NextDownID[row_df])
      //   {
      //     id.push_back(GridID[i]);
      //     down.push_back(down[output_id]);
      //     distance.push_back(count_distance + Length[output_id]);
      //     count_distance = count_distance + Length[output_id];
      //     
      //     output_id++;
      //     id_size = id.size();
      //     
      //     if(output_id >= id_size){
      //       break;
      //     }
      //   }
      // }
      // else{
        auto new_id = find(GridID.begin(), GridID.end(), NextDownID[row_df]) - GridID.begin();
        row_df = new_id;
        row_distances_data++;
        n_connections++;
      // }
    }
    //adding last connection
    if(NextDownID[row_df] == -1 && n_connections != connection_limit && in_memory == false){
      
      id.push_back(GridID[i]);
      down.push_back(NextDownID[row_df]);
      distance.push_back(count_distance + Length[row_df]);
      
      row_distances_data++;
    }
  }
  return DataFrame::create(Named("id") = id,
                           Named("down") = down,
                           Named("distance") = distance);
}