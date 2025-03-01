This is a replication repository for the article *M6 Investment Challenge: The Role of Luck and Strategic Considerations* in IJF.

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
     9. `src\run_competition_simulated_sensitivity.R`
     10. `src\test_IR_level_sensitivity.R`
     11. `src\auxiliary\leaderboard_comparison.R`
     12. `src\auxiliary\strategy_diagram.R`
     13. `src\auxiliary\scaling_effects.R`
     14. `src\auxiliary\position_effects.R`
     15. `src\auxiliary\competition_statistics_simulated.R`
     16. `src\auxiliary\competition_statistics_bootstrapped.R`
     17. `src\auxiliary\test_IR_level_table.R`
     18. `src\auxiliary\competition_statistics_simulated_sensitivity.R`
     19. `src\auxiliary\test_IR_level_table_sensitivity.R`

3. **Compile the PDF**  
   To generate the PDF version of the article, compile the LaTeX file located at `text\article_IJF_1\article_IJF_1.tex`.
