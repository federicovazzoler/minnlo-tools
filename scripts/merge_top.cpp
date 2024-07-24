#include <fstream>
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <sstream>
#include <iomanip> // for std::setprecision
#include <limits>
#include <sys/stat.h>
#include <boost/filesystem.hpp>

// CHECK FILELIST

// check file list sanity
// by checking all items are .top files
// and counting them
int count_files(const std::string& file_list, bool exit_on_error = false) {
  int count = 0;
  std::string line;
  std::ifstream infile(file_list.c_str());
  while (std::getline(infile, line)) {
    if (line.length() < 4 || line.compare(line.length() - 4, 4, ".top") != 0) {
      if (exit_on_error) {
        std::cerr << "Error: invalid file '" << line << "'" << std::endl;
        exit(1);
      }
    } else {
      ++count;
    }
  }

  return count;
}

//

// DEFINE OBJECT

// struct to store the plot metadata
struct PlotData {
  std::string header_row = "";
  std::vector<std::vector<double> > data_rows = {};
};

// function to clear a plot metedata object
PlotData clear(PlotData plot) {
  plot.header_row = "";
  plot.data_rows = {};

  return plot;
}

//

// FILL OBJECT

// function to read an input .top file,
// store all the plots inside in a PlotData object,
// return ai vector of PlotData object
std::vector<PlotData> read_input_file(const std::string& file_path) {
  std::ifstream infile(file_path.c_str());
  std::vector<std::vector<std::string>> data;
  std::vector<std::string> lines;
  std::string line;
  int emptyLines = 0;

  // cycle trought the file
  if (infile.is_open()) {
    while (std::getline(infile, line)) {
      if (line.empty()) {
        emptyLines++;
      } else {
        emptyLines = 0;
      }

      if (line[0] == '#') {
        if (!lines.empty()) {
          data.push_back(lines);
          lines.clear();
        }
      }

      if (emptyLines < 2) {
        lines.push_back(line);
      }

      if (emptyLines == 2) {
        data.push_back(lines);
        lines.clear();
      }
    }

    // If there are still lines in the buffer after the last iteration, push them into the data vector
    if (!lines.empty()) {
      std::cerr << "Still non-empty lines in the end of " << std::endl;
      exit(1);
    }

    // fill plots
    std::vector<PlotData> plots;
    PlotData plot;
    std::string bin_min;
    std::string bin_max;
    std::string bin_xsec;
    std::string bin_err;
    std::vector<double> row;

    for (const auto& v : data) {
      for (const auto& l : v) {
        if (l[0] == '#') {
          plot.header_row = l;
        } else {
          if (!l.empty()) {
            std::string test = l;
            std::replace(test.begin(), test.end(), 'D', 'e');
            std::stringstream ss(test);
            ss >> bin_min >> bin_max >> bin_xsec >> bin_err;
        	row = {};
            row.push_back(std::stod(bin_min));
            row.push_back(std::stod(bin_max));
            row.push_back(std::stod(bin_xsec));
            row.push_back(pow(std::stod(bin_err), 2)); // ATTENTION!
            plot.data_rows.push_back(row);
          }
        }
      }
      plots.push_back(plot);
      plot = clear(plot);
    }
    infile.close();

    return plots;
  } else {
    std::cerr << "Error opening file: " << file_path << std::endl;
    exit(1);
  }
}

//

//
// SUMMING FUNCTIONS
//
PlotData sum_plot(PlotData plot, PlotData plot_to_add) {
  if (plot.header_row == plot_to_add.header_row) {
    if (plot.data_rows.size() == plot_to_add.data_rows.size()) {
      PlotData plot_sum;
      plot_sum.header_row = plot.header_row;
      std::vector<double> row;
      for (unsigned int nrow = 0; nrow < plot.data_rows.size(); nrow++) {
        row = {};
        row.push_back(plot.data_rows[nrow][0]);
        row.push_back(plot.data_rows[nrow][1]);
        row.push_back(plot.data_rows[nrow][2] + plot_to_add.data_rows[nrow][2]);
        // err are already read in quadrature (**2) so we simply add them here, the norm is done in norm_plot
        row.push_back(plot.data_rows[nrow][3] + plot_to_add.data_rows[nrow][3]);
        plot_sum.data_rows.push_back(row);
      }

      return plot_sum;
    } else {
      std::cerr << "Adding plots with different numer of data rows " << plot.header_row << " and " << plot_to_add.header_row << std::endl;
      exit(1);
    }
  } else {
    std::cerr << "Adding plots with different headers " << plot.header_row << " and " << plot_to_add.header_row << std::endl;
    exit(1);
  }
}

std::vector<PlotData> sum_plots(std::vector<PlotData> plots, std::vector<PlotData> plots_to_add) {
  if (plots.size() == plots_to_add.size()) {
    std::vector<PlotData> plots_sum = {};
    for (unsigned int nplot = 0; nplot < plots.size(); nplot++) {
      plots_sum.push_back(sum_plot(plots[nplot], plots_to_add[nplot]));
    }

    return plots_sum;
  } else {
    std::cerr << "Adding plots objects with different sizes" << std::endl;
    exit(1);
  }
}
//
//
//

//
// NORMALISING FUNCTIONS
//
PlotData norm_plot(PlotData plot, int nfiles) {
  if (nfiles > 0) {
      PlotData plot_norm;
      plot_norm.header_row = plot.header_row;
      std::vector<double> row;
      for (unsigned int nrow = 0; nrow < plot.data_rows.size(); nrow++) {
        row = {};
        row.push_back(plot.data_rows[nrow][0]);
        row.push_back(plot.data_rows[nrow][1]);
        row.push_back(plot.data_rows[nrow][2] / (double) nfiles);
        row.push_back(sqrt(plot.data_rows[nrow][3] / pow((double) nfiles, 2)));
        plot_norm.data_rows.push_back(row);
      }

      return plot_norm;
  } else {
      std::cerr << "Trying to normalise " << plot.header_row << " but nfiles is " << nfiles << std::endl;
      exit(1);
  }
}

std::vector<PlotData> norm_plots(std::vector<PlotData> plots, int nfiles) {
  std::vector<PlotData> plots_norm = {};
  for (unsigned int nplot = 0; nplot < plots.size(); nplot++) {
    plots_norm.push_back(norm_plot(plots[nplot], nfiles));
  }

  return plots_norm;
}
//
//
//

//
// DUMP OBJECT ON SCREEN
//
void dump(PlotData plot) {
  //std::cout << std::fixed << bin_xsec << " " << std::setprecision(8) << std::stod(bin_xsec) << std::endl;
  std::cout << "bin_min,bin_max,xsec,err" << std::endl;
  for (unsigned int row = 0; row < plot.data_rows.size(); row++) {
    std::cout << std::fixed << std::setprecision(std::numeric_limits<double>::digits10)
              << plot.data_rows[row][0] << ","
              << plot.data_rows[row][1] << ","
              << plot.data_rows[row][2] << ","
              << plot.data_rows[row][3] << std::endl;
  }
}
//
//
//

//
// DUMP PLOT ON FILE
//
void dump_csv(PlotData plot, const char* out_folder_path, bool verbose) {
  // get plot title from header row
  std::string plot_name = "";
  size_t start_pos = plot.header_row.find("# ") + 2; // find the position of the first delimiter
  size_t end_pos = plot.header_row.find(" index"); // find position of last delimiter

  if (start_pos != std::string::npos && end_pos != std::string::npos) { // if both substrings were found
    plot_name = plot.header_row.substr(start_pos, end_pos - start_pos); // extract the substring between them
  }

  if (plot_name == "") {
    std::cerr << "Unable to find plot name for " << plot.header_row << std::endl;
    exit(1);
  }

  // dump plot
  std::ofstream out_file;
  std::string str(out_folder_path);
  str += "/" + plot_name + ".csv";
  out_file.open(str);

  if (out_file.is_open()) {
     out_file << "bin_min,bin_max,xsec,err" << std::endl;
     for (unsigned int row = 0; row < plot.data_rows.size(); row++) {
       out_file << std::fixed << std::setprecision(std::numeric_limits<double>::digits10)
                << plot.data_rows[row][0] << ","
//       out_file << plot.data_rows[row][0] << ","
                << plot.data_rows[row][1] << ","
                << plot.data_rows[row][2] << ","
                << plot.data_rows[row][3] << std::endl;
     }

     out_file.close();
     if (verbose) std::cout << "Created: " << str << std::endl;
  } else {
    std::cerr << "Error creating file:" << str << std::endl;
    exit(1);
  }
}

void dump_all_plots_to_csv(std::vector<PlotData> plots, const char* out_folder_path, bool verbose){
  for (unsigned int nplot = 0; nplot < plots.size(); nplot++) {
    dump_csv(plots[nplot], out_folder_path, verbose);
  }
}

void dump_all_plots_to_top(std::vector<PlotData> plots, const char* out_folder_path, bool verbose){
  // create output file
  std::ofstream out_file;
  std::string str(out_folder_path);
  str += "/merged.top";
  out_file.open(str);

  if (out_file.is_open()) {
    PlotData plot;

    for (unsigned int nplot = 0; nplot < plots.size(); nplot++) {
      plot = plots[nplot];
      out_file << plot.header_row << std::endl;
      for (unsigned int row = 0; row < plot.data_rows.size(); row++) {
        out_file << std::fixed << std::scientific << std::setprecision(std::numeric_limits<double>::digits10)
                 << plot.data_rows[row][0] << " "
                 << plot.data_rows[row][1] << " "
                 << plot.data_rows[row][2] << " "
                 << plot.data_rows[row][3] << std::endl;
      }

      out_file << "" << std::endl;
      out_file << "" << std::endl;
    }

     out_file.close();
     if (verbose) std::cout << "Created: " << str << std::endl;
  } else {
    std::cerr << "Error creating file:" << str << std::endl;
    exit(1);
  }
}

//
//
//

// MAIN

int main(int argc, char* argv[]) {
  if (argc < 3) {
    std::cout << "Usage: " << argv[0] << " out_folder_path file_list_path" << std::endl;
    exit(1);
  }

  // input variables
  const char* out_folder_path = argv[1];
  std::string file_list_path = argv[2];
  bool verbose = true;

  // count files to merge
  int nfiles = count_files(file_list_path, true);

  // cycle trough files
  std::ifstream file_list(file_list_path.c_str());
  std::string file_path;
  std::vector<PlotData> plots;
  int ifile = 1;

  //// open first file and store all plots in `plots`
  std::getline(file_list, file_path);
  plots = read_input_file(file_path);
  if (verbose) std::cout << "\rAdding file: " << ifile << "/" << nfiles << "\033[K" << std::flush;
  ifile++;

  //// if `nfiles > 1` proceed reading and adding
  if (nfiles > 1) {
    std::vector<PlotData> plots_to_add;
    while (std::getline(file_list, file_path)) {
      if (verbose) std::cout << "\rAdding file: " << ifile << "/" << nfiles << "\033[K" << std::flush;
      plots_to_add = read_input_file(file_path);
      plots = sum_plots(plots, plots_to_add);
      ifile++;
    }
    if (verbose) std::cout << std::endl;
  }

  //// normalise plots
  plots = norm_plots(plots, nfiles);

  // create output folder
  // check if directory already exists
  if (boost::filesystem::is_directory(out_folder_path)) {
    std::cerr << "Error creating output directory: \"" << out_folder_path << "\" already exists!" << std::endl;
    exit(1);
  }

  // create directory
  boost::system::error_code ec;
  boost::filesystem::create_directories(out_folder_path, ec);
  if (ec) {
      std::cerr << "Failed to create \"" << out_folder_path << "\": " << ec.message() << std::endl;
      return -1;
  }

  // dump `plots` in separate csv files
  dump_all_plots_to_csv(plots, out_folder_path, verbose = false);
  // dump `plots` in a single `.top` file (to be used for additional merging)
  dump_all_plots_to_top(plots, out_folder_path, verbose = false);
  std::cout << "The merged files are stored in: " << out_folder_path << std::endl;

  return 0;
}
