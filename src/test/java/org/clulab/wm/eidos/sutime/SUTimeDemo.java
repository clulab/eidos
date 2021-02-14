package org.clulab.wm.eidos.sutime;

// See https://nlp.stanford.edu/software/sutime.shtml .

import java.util.List;
import java.util.Properties;

import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.*;
import edu.stanford.nlp.time.*;
import edu.stanford.nlp.util.CoreMap;

public class SUTimeDemo {

	/** Example usage:
	 *  java SUTimeDemo "Three interesting dates are 18 Feb 1997, the 20th of july and 4 days from today."
	 *
	 *  @param args Strings to interpret
	 */
	public static void main(String[] args) {
		Properties props = new Properties();
		AnnotationPipeline pipeline = new AnnotationPipeline();
		pipeline.addAnnotator(new TokenizerAnnotator(false));
		pipeline.addAnnotator(new WordsToSentencesAnnotator(false));
		pipeline.addAnnotator(new POSTaggerAnnotator(false));
		pipeline.addAnnotator(new TimeAnnotator("sutime", props));

		for (String text : args) {
			Annotation annotation = new Annotation(text);
			annotation.set(CoreAnnotations.DocDateAnnotation.class, "2013-07-14");
			pipeline.annotate(annotation);
			System.out.println(annotation.get(CoreAnnotations.TextAnnotation.class));
			List<CoreMap> timexAnnsAll = annotation.get(TimeAnnotations.TimexAnnotations.class);
			for (CoreMap cm : timexAnnsAll) {
				List<CoreLabel> tokens = cm.get(CoreAnnotations.TokensAnnotation.class);
				System.out.println(cm + " [from char offset " +
						tokens.get(0).get(CoreAnnotations.CharacterOffsetBeginAnnotation.class) +
						" to " + tokens.get(tokens.size() - 1).get(CoreAnnotations.CharacterOffsetEndAnnotation.class) + ']' +
						" --> " + cm.get(TimeExpression.Annotation.class).getTemporal());
			}
			System.out.println("--");
		}
	}
}
