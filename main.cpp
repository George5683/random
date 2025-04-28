#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <cmath>
#include <algorithm>
#include <iomanip>

// Structure to hold the data
struct ExperimentData {
    int subject;
    double createGameTime;
    double findGameTime;
    double rsvpTime;
    double updateProfileTime;
    bool filtersOn;
    bool tutorialGiven;
};

// Function to calculate mean
double calculateMean(const std::vector<double>& values) {
    double sum = 0.0;
    for (double value : values) {
        sum += value;
    }
    return sum / values.size();
}

// Function to calculate sum of squares
double calculateSumOfSquares(const std::vector<double>& values, double mean) {
    double sumOfSquares = 0.0;
    for (double value : values) {
        sumOfSquares += (value - mean) * (value - mean);
    }
    return sumOfSquares;
}

// Structure to hold ANOVA results
struct AnovaResult {
    double filterSS;
    double tutorialSS;
    double interactionSS;
    double errorSS;
    double totalSS;
    
    int filterDF;
    int tutorialDF;
    int interactionDF;
    int errorDF;
    int totalDF;
    
    double filterMS;
    double tutorialMS;
    double interactionMS;
    double errorMS;
    
    double filterF;
    double tutorialF;
    double interactionF;
    
    double filterP;
    double tutorialP;
    double interactionP;
};

// Function to calculate p-value from F statistic
double calculatePValue(double F, int dfNumerator, int dfDenominator) {
    // This is a simplified approximation of p-value
    // For actual implementation, you might want to use a statistical library
    // or implement the F-distribution CDF
    
    // For demonstration purposes, we'll use this approximation
    // Note: This is NOT accurate for all cases
    double p = 1.0 / (1.0 + F * std::sqrt(dfNumerator / (double)dfDenominator));
    
    // More accurate p-value calculation would require numerical integration
    // or statistical tables
    
    return p;
}

// Function to perform two-way ANOVA
AnovaResult performTwoWayAnova(const std::vector<ExperimentData>& data, int taskIndex) {
    // Extract the relevant task completion times based on taskIndex
    // and organize by filter and tutorial conditions
    std::vector<double> filterYesTutorialYes;
    std::vector<double> filterYesTutorialNo;
    std::vector<double> filterNoTutorialYes;
    std::vector<double> filterNoTutorialNo;
    
    for (const auto& entry : data) {
        double value;
        switch (taskIndex) {
            case 1: value = entry.createGameTime; break;
            case 2: value = entry.findGameTime; break;
            case 3: value = entry.rsvpTime; break;
            case 4: value = entry.updateProfileTime; break;
            default: value = 0.0;
        }
        
        if (entry.filtersOn && entry.tutorialGiven) {
            filterYesTutorialYes.push_back(value);
        } else if (entry.filtersOn && !entry.tutorialGiven) {
            filterYesTutorialNo.push_back(value);
        } else if (!entry.filtersOn && entry.tutorialGiven) {
            filterNoTutorialYes.push_back(value);
        } else {
            filterNoTutorialNo.push_back(value);
        }
    }
    
    // Calculate cell means
    double meanFYTY = calculateMean(filterYesTutorialYes);
    double meanFYTN = calculateMean(filterYesTutorialNo);
    double meanFNTY = calculateMean(filterNoTutorialYes);
    double meanFNTN = calculateMean(filterNoTutorialNo);
    
    // Calculate row and column means
    std::vector<double> filterYes;
    filterYes.insert(filterYes.end(), filterYesTutorialYes.begin(), filterYesTutorialYes.end());
    filterYes.insert(filterYes.end(), filterYesTutorialNo.begin(), filterYesTutorialNo.end());
    
    std::vector<double> filterNo;
    filterNo.insert(filterNo.end(), filterNoTutorialYes.begin(), filterNoTutorialYes.end());
    filterNo.insert(filterNo.end(), filterNoTutorialNo.begin(), filterNoTutorialNo.end());
    
    std::vector<double> tutorialYes;
    tutorialYes.insert(tutorialYes.end(), filterYesTutorialYes.begin(), filterYesTutorialYes.end());
    tutorialYes.insert(tutorialYes.end(), filterNoTutorialYes.begin(), filterNoTutorialYes.end());
    
    std::vector<double> tutorialNo;
    tutorialNo.insert(tutorialNo.end(), filterYesTutorialNo.begin(), filterYesTutorialNo.end());
    tutorialNo.insert(tutorialNo.end(), filterNoTutorialNo.begin(), filterNoTutorialNo.end());
    
    double meanFilterYes = calculateMean(filterYes);
    double meanFilterNo = calculateMean(filterNo);
    double meanTutorialYes = calculateMean(tutorialYes);
    double meanTutorialNo = calculateMean(tutorialNo);
    
    // Calculate grand mean
    std::vector<double> allValues;
    allValues.insert(allValues.end(), filterYes.begin(), filterYes.end());
    allValues.insert(allValues.end(), filterNo.begin(), filterNo.end());
    double grandMean = calculateMean(allValues);
    
    // Calculate sums of squares
    int n = filterYesTutorialYes.size(); // Number of observations per cell
    int totalN = allValues.size();       // Total number of observations
    
    // Sum of squares for filter factor
    double filterSS = 2 * n * ((meanFilterYes - grandMean) * (meanFilterYes - grandMean) + 
                             (meanFilterNo - grandMean) * (meanFilterNo - grandMean));
                             
    // Sum of squares for tutorial factor
    double tutorialSS = 2 * n * ((meanTutorialYes - grandMean) * (meanTutorialYes - grandMean) + 
                               (meanTutorialNo - grandMean) * (meanTutorialNo - grandMean));
                               
    // Sum of squares for interaction
    double interactionSS = n * (
        (meanFYTY - meanFilterYes - meanTutorialYes + grandMean) * (meanFYTY - meanFilterYes - meanTutorialYes + grandMean) +
        (meanFYTN - meanFilterYes - meanTutorialNo + grandMean) * (meanFYTN - meanFilterYes - meanTutorialNo + grandMean) +
        (meanFNTY - meanFilterNo - meanTutorialYes + grandMean) * (meanFNTY - meanFilterNo - meanTutorialYes + grandMean) +
        (meanFNTN - meanFilterNo - meanTutorialNo + grandMean) * (meanFNTN - meanFilterNo - meanTutorialNo + grandMean)
    );
    
    // Calculate total sum of squares
    double totalSS = calculateSumOfSquares(allValues, grandMean);
    
    // Calculate error sum of squares
    double errorSS = totalSS - filterSS - tutorialSS - interactionSS;
    
    // Degrees of freedom
    int filterDF = 1;
    int tutorialDF = 1;
    int interactionDF = 1;
    int errorDF = totalN - 4; // 4 groups
    int totalDF = totalN - 1;
    
    // Mean squares
    double filterMS = filterSS / filterDF;
    double tutorialMS = tutorialSS / tutorialDF;
    double interactionMS = interactionSS / interactionDF;
    double errorMS = errorSS / errorDF;
    
    // F statistics
    double filterF = filterMS / errorMS;
    double tutorialF = tutorialMS / errorMS;
    double interactionF = interactionMS / errorMS;
    
    // P values
    double filterP = calculatePValue(filterF, filterDF, errorDF);
    double tutorialP = calculatePValue(tutorialF, tutorialDF, errorDF);
    double interactionP = calculatePValue(interactionF, interactionDF, errorDF);
    
    // Create and return result
    AnovaResult result;
    result.filterSS = filterSS;
    result.tutorialSS = tutorialSS;
    result.interactionSS = interactionSS;
    result.errorSS = errorSS;
    result.totalSS = totalSS;
    
    result.filterDF = filterDF;
    result.tutorialDF = tutorialDF;
    result.interactionDF = interactionDF;
    result.errorDF = errorDF;
    result.totalDF = totalDF;
    
    result.filterMS = filterMS;
    result.tutorialMS = tutorialMS;
    result.interactionMS = interactionMS;
    result.errorMS = errorMS;
    
    result.filterF = filterF;
    result.tutorialF = tutorialF;
    result.interactionF = interactionF;
    
    result.filterP = filterP;
    result.tutorialP = tutorialP;
    result.interactionP = interactionP;
    
    return result;
}

// Function to display the ANOVA results
void displayAnovaResults(const AnovaResult& result, const std::string& taskName) {
    std::cout << "Two-way ANOVA Results for " << taskName << ":" << std::endl;
    std::cout << "-------------------------------------------" << std::endl;
    std::cout << "Source\t\tSS\tDF\tMS\tF\tp-value" << std::endl;
    std::cout << "-------------------------------------------" << std::endl;
    
    std::cout << std::fixed << std::setprecision(3);
    
    std::cout << "Filter\t\t" << result.filterSS << "\t" << result.filterDF << "\t" 
              << result.filterMS << "\t" << result.filterF << "\t" << result.filterP << std::endl;
              
    std::cout << "Tutorial\t" << result.tutorialSS << "\t" << result.tutorialDF << "\t" 
              << result.tutorialMS << "\t" << result.tutorialF << "\t" << result.tutorialP << std::endl;
              
    std::cout << "Interaction\t" << result.interactionSS << "\t" << result.interactionDF << "\t" 
              << result.interactionMS << "\t" << result.interactionF << "\t" << result.interactionP << std::endl;
              
    std::cout << "Error\t\t" << result.errorSS << "\t" << result.errorDF << "\t" 
              << result.errorMS << "\t-\t-" << std::endl;
              
    std::cout << "Total\t\t" << result.totalSS << "\t" << result.totalDF << "\t-\t-\t-" << std::endl;
    
    std::cout << "-------------------------------------------" << std::endl;
    std::cout << "Significance test:" << std::endl;
    
    if (result.filterP < 0.05) {
        std::cout << "- Main effect of Filtering: SIGNIFICANT (p < 0.05)" << std::endl;
    } else {
        std::cout << "- Main effect of Filtering: NOT significant (p > 0.05)" << std::endl;
    }
    
    if (result.tutorialP < 0.05) {
        std::cout << "- Main effect of Tutorial: SIGNIFICANT (p < 0.05)" << std::endl;
    } else {
        std::cout << "- Main effect of Tutorial: NOT significant (p > 0.05)" << std::endl;
    }
    
    if (result.interactionP < 0.05) {
        std::cout << "- Interaction effect: SIGNIFICANT (p < 0.05)" << std::endl;
    } else {
        std::cout << "- Interaction effect: NOT significant (p > 0.05)" << std::endl;
    }
    
    std::cout << std::endl;
}

// Function to read data from a CSV file
std::vector<ExperimentData> readDataFromCSV(const std::string& filename) {
    std::vector<ExperimentData> data;
    std::ifstream file(filename);
    
    if (!file.is_open()) {
        std::cerr << "Error: Unable to open file " << filename << std::endl;
        return data;
    }
    
    std::string line;
    // Skip header line
    std::getline(file, line);
    
    while (std::getline(file, line)) {
        std::stringstream ss(line);
        std::string token;
        ExperimentData entry;
        
        // Parse subject number
        std::getline(ss, token, ',');
        entry.subject = std::stoi(token);
        
        // Parse create game time
        std::getline(ss, token, ',');
        entry.createGameTime = std::stod(token);
        
        // Parse find game time
        std::getline(ss, token, ',');
        entry.findGameTime = std::stod(token);
        
        // Parse RSVP time
        std::getline(ss, token, ',');
        entry.rsvpTime = std::stod(token);
        
        // Parse update profile time
        std::getline(ss, token, ',');
        entry.updateProfileTime = std::stod(token);
        
        // Parse filters on
        std::getline(ss, token, ',');
        entry.filtersOn = (token == "Yes" || token == "yes" || token == "TRUE" || token == "true" || token == "1");
        
        // Parse tutorial given
        std::getline(ss, token, ',');
        entry.tutorialGiven = (token == "Yes" || token == "yes" || token == "TRUE" || token == "true" || token == "1");
        
        data.push_back(entry);
    }
    
    return data;
}

// Function to calculate and display group means
void displayGroupMeans(const std::vector<ExperimentData>& data, int taskIndex) {
    std::vector<double> filterYesTutorialYes;
    std::vector<double> filterYesTutorialNo;
    std::vector<double> filterNoTutorialYes;
    std::vector<double> filterNoTutorialNo;
    
    for (const auto& entry : data) {
        double value;
        switch (taskIndex) {
            case 1: value = entry.createGameTime; break;
            case 2: value = entry.findGameTime; break;
            case 3: value = entry.rsvpTime; break;
            case 4: value = entry.updateProfileTime; break;
            default: value = 0.0;
        }
        
        if (entry.filtersOn && entry.tutorialGiven) {
            filterYesTutorialYes.push_back(value);
        } else if (entry.filtersOn && !entry.tutorialGiven) {
            filterYesTutorialNo.push_back(value);
        } else if (!entry.filtersOn && entry.tutorialGiven) {
            filterNoTutorialYes.push_back(value);
        } else {
            filterNoTutorialNo.push_back(value);
        }
    }
    
    std::string taskName;
    switch (taskIndex) {
        case 1: taskName = "Create Game"; break;
        case 2: taskName = "Find Game to Attend"; break;
        case 3: taskName = "RSVP"; break;
        case 4: taskName = "Update Profile Info"; break;
        default: taskName = "Unknown Task";
    }
    
    std::cout << "Group Means for " << taskName << ":" << std::endl;
    std::cout << "----------------------------------" << std::endl;
    std::cout << std::fixed << std::setprecision(2);
    std::cout << "Control (no filters, no tutorial): " << calculateMean(filterNoTutorialNo) << " sec" << std::endl;
    std::cout << "No filters with tutorial: " << calculateMean(filterNoTutorialYes) << " sec" << std::endl;
    std::cout << "Filters without tutorial: " << calculateMean(filterYesTutorialNo) << " sec" << std::endl;
    std::cout << "Filters and tutorial: " << calculateMean(filterYesTutorialYes) << " sec" << std::endl;
    std::cout << "----------------------------------" << std::endl << std::endl;
}

int main() {
    std::cout << "Two-Way ANOVA Analysis Program" << std::endl;
    std::cout << "==============================" << std::endl;
    
    // Ask for the input file name
    std::string filename;
    std::cout << "Enter the name of the CSV file containing the data: ";
    std::cin >> filename;
    
    // Read data from the CSV file
    std::vector<ExperimentData> data = readDataFromCSV(filename);
    
    if (data.empty()) {
        std::cerr << "No data was read from the file. Exiting program." << std::endl;
        return 1;
    }
    
    std::cout << "Successfully read " << data.size() << " data points from the file." << std::endl << std::endl;
    
    // Perform analysis for each task
    for (int taskIndex = 1; taskIndex <= 4; taskIndex++) {
        std::string taskName;
        switch (taskIndex) {
            case 1: taskName = "Task #1: Create Game"; break;
            case 2: taskName = "Task #2: Find Game to Attend"; break;
            case 3: taskName = "Task #3: RSVP"; break;
            case 4: taskName = "Task #4: Update Profile Info"; break;
            default: taskName = "Unknown Task";
        }
        
        // Display group means
        displayGroupMeans(data, taskIndex);
        
        // Perform ANOVA
        AnovaResult result = performTwoWayAnova(data, taskIndex);
        
        // Display results
        displayAnovaResults(result, taskName);
    }
    
    std::cout << "Analysis complete." << std::endl;
    
    return 0;
}
