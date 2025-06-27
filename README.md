# appAIscreenR
appAIscreenR is an interactive Shiny application designed to streamline the process of uploading data, 
running the AIscreenR AI-model on the data, review the results and download the output. The app currently supports 
only the AIscreenR-model which is a screening tool for systematic reviewing. See: https://github.com/MikkelVembye/AIscreenR


## Features:
- **Data Upload**: Upload data files in various formats for processing.
- **Run AIscreenR AI Model**: Execute the AI model AIscreenR with user-defined inputs such as API keys and prompts.
- **Interactive Results Overview**: Explore the model’s output using interactive tables.
- **Save & Download**: Save and download results in multiple file formats.
- **Modular Design**: Built using reusable UI and server modules for easy maintenance.

#### The overview tab:
The overview tab contains different datatable that can be chosen: 

- **Dataframe of differences**: A table showing the articles where the AI's decision differs from the human decision
- **Answer Data**: A combined dataset containg the original input along with both the AI's and the human's inclusion/exclusion decisions
- **Price Data**: The cost of running the AI-model
- **Result object**: 
- **Performance Data**: How good the model performs
- **Confusion Matrix**: A table of numbers showing how often the AI model agrees or disagrees with the human decisions for example, how many times they both included an item, both excluded it, or disagreed. 


#### How to run the app
1. Open R or Rstudio
3. Run the following line to load and start the app directly from GitHub

```r
shiny::runGitHub("appAIscreenR", "AUL-BSS-Datalab")
```

