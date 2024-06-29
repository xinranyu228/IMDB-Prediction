# IMDb Rating Prediction Project

## Project Overview
This project aims to develop a statistical model to forecast the IMDb ratings for twelve upcoming films scheduled for release in November 2023. Using a dataset of approximately two thousand films, the project identifies significant predictors of IMDb ratings and constructs an accurate predictive model using regression techniques. The model is trained and tuned through cross-validation to maximize out-of-sample predictive accuracy while avoiding overfitting. Feature engineering and selection methods are systematically explored to construct an optimal set of predictors. The final model generates predicted IMDb ratings for the twelve films of interest, which are then evaluated against actual post-release IMDb ratings to assess predictive accuracy.

## Data Description
The dataset includes information on two thousand movies, with variables such as budget, release date, genres, and cast. The primary target variable is the IMDb score, which ranges from 1 to 10. Key independent variables include:
- **Budget**: Film production budget in USD.
- **Release Year**: Year of movie release.
- **Duration**: Runtime of the movie in minutes.
- **Actor Star Meters**: Fame levels of the top-billed actors.
- **Number of Faces in Poster**: Count of cast member faces pictured on the movie's poster.
- **IMDbPro Ranking**: 2023 ranking of the movie based on page views and clicks.
- **Number of News Articles**: Count of online news articles about the film.
- **Maturity Rating**: MPAA rating assigned to the movie.
- **Genre**: Main genre categorization of the movie.

## Model Selection and Methodology
The project employs linear regression to predict IMDb scores based on key movie attributes. The modeling process includes:
- **Polynomial and Spline Regression**: To capture non-linear relationships.
- **Feature Selection**: Based on R-squared metrics and p-values.
- **Cross-Validation**: To ensure robust model performance and prevent overfitting.
- **Heteroskedasticity Assessment**: Using the NCV test and robust covariance matrix computation.

### Final Model and Results
The final model achieved an R-squared value of 0.4091 and a mean squared error (MSE) of 0.7210. Predictions for the twelve upcoming films ranged from 5.55 to 7.66. Key predictors in the final model included:
- **Distributor Rating**: Positive correlation with IMDb score.
- **Quadratic Duration**: Inverted-U shaped relationship with IMDb score.
- **Drama Genre Indicator**: Higher scores for dramas.
- **Interaction Term**: Between release year and distributor rating.

## Conclusion
The developed model provides reasonably accurate predictions of IMDb scores for new films, explaining approximately 41% of the variance in scores. Future improvements could include expanding the dataset and incorporating more comprehensive predictors to enhance predictive accuracy.

