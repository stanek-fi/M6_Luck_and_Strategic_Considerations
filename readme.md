This is a replication repository for the article *[M6 Investment Challenge: The Role of Luck and Strategic Considerations](https://arxiv.org/abs/2412.04490v1)*.

To replicate the analysis:

1. **Install dependencies**  
   Install all required dependencies listed in `requirements.txt`.

2. **Run the analysis**  
   You can run the analysis using either of the following methods:

   - **Using DVC**:  
    If you are a DVC user, simply run `dvc exp run` in the console or execute the `run.bat` file.
   - **Manual Execution**:  
    Alternatively, you can also run all scripts manually in the following order:

     1. `src\process_data.R`
     2. `src\estimate_coefficients.R`
     3. `src\compute_strategy.R`
     4. `src\run_competition_simulated.R`
     5. `src\run_competition_bootstrapped.R`
     6. `src\test_IR_level.R`
     7. `src\test_IR_pvalue.R`
     8. `src\submission_analysis.R`
     9. `src\auxiliary\leaderboard_comparison.R`
     10. `src\auxiliary\strategy_diagram.R`
     11. `src\auxiliary\scaling_effects.R`
     12. `src\auxiliary\position_effects.R`
     13. `src\auxiliary\competition_statistics_simulated.R`
     14. `src\auxiliary\competition_statistics_bootstrapped.R`
     15. `src\auxiliary\test_IR_level_table.R`

3. **Compile the PDF**  
   To generate the PDF version of the article, compile the LaTeX file located at `text\article_arxiv\article_arxiv.tex`.
