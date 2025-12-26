use crate::protocol::MatchResult;
use nucleo_matcher::{
    pattern::{CaseMatching, Normalization, Pattern},
    Config, Matcher, Utf32Str,
};

/// Fuzzy matcher using Nucleo
pub struct FuzzyMatcher {
    matcher: Matcher,
    buf: Vec<char>,
}

impl FuzzyMatcher {
    pub fn new() -> Self {
        Self {
            matcher: Matcher::new(Config::DEFAULT),
            buf: Vec::with_capacity(1024),
        }
    }

    /// Match a query against a list of candidates, returning sorted results
    pub fn match_list(
        &mut self,
        query: &str,
        candidates: &[String],
        max_results: usize,
    ) -> Vec<MatchResult> {
        if query.is_empty() {
            // Empty query returns all candidates unscored
            return candidates
                .iter()
                .take(max_results)
                .enumerate()
                .map(|(index, text)| MatchResult {
                    index,
                    score: 0,
                    text: text.clone(),
                    indices: vec![],
                })
                .collect();
        }

        let pattern = Pattern::parse(query, CaseMatching::Smart, Normalization::Smart);
        let mut results = Vec::new();

        for (index, candidate) in candidates.iter().enumerate() {
            self.buf.clear();
            let haystack = Utf32Str::new(candidate, &mut self.buf);

            if let Some(score) = pattern.score(haystack, &mut self.matcher) {
                let mut indices = Vec::new();
                pattern.indices(haystack, &mut self.matcher, &mut indices);

                results.push(MatchResult {
                    index,
                    score,
                    text: candidate.clone(),
                    indices,
                });
            }
        }

        // Sort by score descending
        results.sort_by(|a, b| b.score.cmp(&a.score));
        results.truncate(max_results);
        results
    }
}

impl Default for FuzzyMatcher {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_matching() {
        let mut matcher = FuzzyMatcher::new();
        let candidates = vec![
            "src/main.rs".to_string(),
            "src/matcher.rs".to_string(),
            "test/main_test.rs".to_string(),
        ];

        let results = matcher.match_list("main", &candidates, 10);
        assert!(!results.is_empty());
        assert!(results[0].text.contains("main"));
    }

    #[test]
    fn test_empty_query() {
        let mut matcher = FuzzyMatcher::new();
        let candidates = vec!["foo.txt".to_string(), "bar.txt".to_string()];

        let results = matcher.match_list("", &candidates, 10);
        assert_eq!(results.len(), 2);
        assert_eq!(results[0].score, 0);
    }

    #[test]
    fn test_max_results() {
        let mut matcher = FuzzyMatcher::new();
        let candidates: Vec<String> = (0..100).map(|i| format!("file{}.txt", i)).collect();

        let results = matcher.match_list("file", &candidates, 10);
        assert_eq!(results.len(), 10);
    }

    #[test]
    fn test_unicode() {
        let mut matcher = FuzzyMatcher::new();
        let candidates = vec![
            "日本語.txt".to_string(),
            "test.txt".to_string(),
            "émoji🎉.rs".to_string(),
        ];

        let results = matcher.match_list("日本", &candidates, 10);
        assert!(!results.is_empty());
    }
}
