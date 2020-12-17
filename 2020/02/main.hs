
import Network.HTTP

main = do
  response <- simpleHTTP $ getRequest "https://adventofcode.com/2020/day/2/input"
  let body = fmap rspBody response
  print response
  print body

