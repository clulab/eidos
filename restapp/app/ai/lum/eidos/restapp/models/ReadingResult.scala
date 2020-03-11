package ai.lum.eidos.restapp.models

import java.time.LocalDateTime

class ReadingResult {
  val timestamp = LocalDateTime.now()
}

class ReceivedReadingResult extends ReadingResult {

}

class SucceededReadingResult extends ReadingResult {

}

class FailedReadingResult extends ReadingResult {

}

class ExpiredReadingResult extends ReadingResult {

}