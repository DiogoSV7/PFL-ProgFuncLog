transpose:: [[a]]->[[a]]
transpose [] = []
transpose ([]:_) = []
transpose xss = [head xs | xs <- xss] : transpose [tail xs | xs <- xss]