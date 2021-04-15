# uk rest api test


endpoint <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=region&structure={"date":"date","newCases":"newCasesBySpecimenDate","areaName":"areaName","cumCases":"cumCasesBySpecimenDate","newDeaths":"newDeathsByDeathDate","cumDeaths":"cumDeathsByDeathDate"}'

endpoint <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=utla&structure={"date":"date","newCases":"newCasesBySpecimenDate","areaName":"areaName","cumCases":"cumCasesBySpecimenDate","newDeaths":"newDeathsByDeathDate"}&page=2'
endpoint <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=utla;areaName=Northamptonshire&structure={"date":"date","newCases":"newCasesBySpecimenDate","areaName":"areaName","cumCases":"cumCasesBySpecimenDate","newDeaths":"newDeathsByDeathDate"}'

httr::GET(
  url = endpoint,
  httr::timeout(10)
) -> response


if (response$status_code >= 400) {
  err_msg = httr::http_status(response)
  stop(err_msg)
}

# Convert response from binary to JSON:
json_text <- httr::content(response, "text")
data      <- jsonlite::fromJSON(json_text)

print(data)
